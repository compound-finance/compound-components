module CompoundComponents.Utils.Renderer exposing (renderContents, renderNav, toVirtualDom)

import CompoundComponents.Utils.CompoundHtmlAttributes as CompoundHtmlAttributes exposing (HrefLinkType(..), href)
import CompoundComponents.Utils.Highlighter as Highlighter exposing (highlight)
import CompoundComponents.Utils.Markdown.Table exposing (Table(..))
import CompoundComponents.Utils.Markdown.TableParser as TableParser
import Html exposing (..)
import Html.Attributes as Attr
import Html.Parser exposing (Node(..))
import Markdown.Block as Block
import Markdown.Html
import Markdown.Parser as Markdown
import Markdown.Renderer as Renderer exposing (Renderer, defaultHtmlRenderer)
import Parser.Advanced as ParserAdvanced


renderNav : String -> List (Html msg)
renderNav str =
    str
        |> render navRenderer


renderContents : String -> List (Html msg)
renderContents str =
    str
        |> render htmlRenderer


render : Renderer (Html msg) -> String -> List (Html msg)
render renderer str =
    let
        deadEndsToString deadEnds =
            deadEnds
                |> List.map Markdown.deadEndToString
                |> String.join "\n"
    in
    str
        |> Markdown.parse
        |> Result.mapError deadEndsToString
        |> Result.andThen (\ast -> Renderer.render renderer ast)
        |> Result.withDefault [ Html.pre [] [ Html.text str ] ]


renderLink : String -> Maybe String -> List (Html msg) -> Html msg
renderLink destination maybeTitle contents =
    -- Note: we ignore title for now
    let
        titleAttrs =
            maybeTitle
                |> Maybe.map (\title -> [ Attr.title title ])
                |> Maybe.withDefault []

        ( navigationType, otherAttrs ) =
            if String.startsWith "#" destination then
                ( PageNavigation, [] )

            else if String.startsWith "http" destination then
                ( External, [ Attr.target "_blank" ] )

            else
                ( Internal, [] )
    in
    Html.a
        (titleAttrs
            ++ otherAttrs
            ++ CompoundHtmlAttributes.href navigationType destination
        )
        contents


htmlRenderer : Renderer (Html msg)
htmlRenderer =
    { defaultHtmlRenderer
        | heading =
            \({ level, children } as headingMap) ->
                case level of
                    Block.H1 ->
                        Html.label [ Attr.class "big" ] children

                    Block.H2 ->
                        Html.h2 [ Attr.class "subheader" ] children

                    _ ->
                        defaultHtmlRenderer.heading headingMap
        , codeSpan =
            \content ->
                case content of
                    "msg.value" ->
                        Html.span [ Attr.class "inline-code" ]
                            [ Html.text content
                            , Html.span [ Attr.class "payable" ]
                                [ Html.text "payable" ]
                            ]

                    "2^256" ->
                        Html.span []
                            [ Html.text "2"
                            , Html.sup []
                                [ Html.text "256" ]
                            ]

                    _ ->
                        Html.span [ Attr.class "inline-code" ] [ Html.text content ]
        , link =
            \link content ->
                renderLink link.destination link.title content
        , unorderedList =
            \items ->
                Html.div [ Attr.class "ul-container" ]
                    [ Html.ul []
                        (items
                            |> List.map
                                (\item ->
                                    case item of
                                        Block.ListItem task children ->
                                            let
                                                checkbox =
                                                    case task of
                                                        Block.NoTask ->
                                                            Html.text ""

                                                        Block.IncompleteTask ->
                                                            Html.input
                                                                [ Attr.disabled True
                                                                , Attr.checked False
                                                                , Attr.type_ "checkbox"
                                                                ]
                                                                []

                                                        Block.CompletedTask ->
                                                            Html.input
                                                                [ Attr.disabled True
                                                                , Attr.checked True
                                                                , Attr.type_ "checkbox"
                                                                ]
                                                                []
                                            in
                                            Html.li [] (checkbox :: children)
                                )
                        )
                    ]
        , html = sectionHtml
        , codeBlock =
            \{ body, language } ->
                if body == "" then
                    text ""

                else
                    case language of
                        Just "sh" ->
                            Highlighter.highlight body

                        Just "bash" ->
                            Highlighter.highlight body

                        Just "js" ->
                            Highlighter.highlight body

                        Just "javascript" ->
                            Highlighter.highlight body

                        Just "solc" ->
                            Highlighter.highlightSol body

                        Just "solidity" ->
                            Highlighter.highlightSol body

                        Just "note" ->
                            Html.p [ Attr.class "notice" ]
                                (renderContents body)

                        Just languageString ->
                            if String.startsWith "pseudo-table:" languageString then
                                let
                                    tableClass =
                                        String.replace "pseudo-table:" "" languageString

                                    markdownTable =
                                        ParserAdvanced.run TableParser.parser body
                                in
                                case markdownTable of
                                    Ok (Table headerColumns rows) ->
                                        Html.table [ Attr.class tableClass ]
                                            [ Html.thead []
                                                [ Html.tr []
                                                    (headerColumns
                                                        |> List.map
                                                            (\{ label, alignment } ->
                                                                Html.th [] [ Html.text label ]
                                                            )
                                                    )
                                                ]
                                            , Html.tbody []
                                                (rows
                                                    |> List.map
                                                        (\rowData ->
                                                            Html.tr []
                                                                (rowData
                                                                    |> List.map
                                                                        (\rowColumnData ->
                                                                            Html.td [] (renderContents rowColumnData)
                                                                        )
                                                                )
                                                        )
                                                )
                                            ]

                                    _ ->
                                        Html.pre [] [ Html.text body ]

                            else
                                Html.pre [] [ Html.text body ]

                        _ ->
                            Html.pre [] [ Html.text body ]
    }


empty0 : Html msg
empty0 =
    Html.text ""


empty : a -> Html msg
empty =
    \x -> empty0


empty2 : a -> b -> Html msg
empty2 =
    \x y -> empty0


sectionHtml : Markdown.Html.Renderer (List (Html msg) -> Html msg)
sectionHtml =
    Markdown.Html.oneOf
        [ Markdown.Html.tag "section"
            (\id renderedChildren ->
                Html.section [ Attr.id id ] renderedChildren
            )
            |> Markdown.Html.withAttribute "id"
        , Markdown.Html.tag "section"
            (\class renderedChildren ->
                Html.section [ Attr.class class ] renderedChildren
            )
            |> Markdown.Html.withAttribute "class"
        , Markdown.Html.tag "div"
            (\id renderedChildren ->
                Html.div [ Attr.id id ] renderedChildren
            )
            |> Markdown.Html.withAttribute "id"
        , Markdown.Html.tag "div"
            (\renderedChildren ->
                Html.div [] renderedChildren
            )
        ]


navRenderer : Renderer (Html msg)
navRenderer =
    { heading = empty
    , paragraph = empty
    , hardLineBreak = empty0
    , blockQuote = empty
    , strong = empty
    , emphasis = empty
    , codeSpan = empty
    , link = empty2
    , image = empty
    , text = empty
    , unorderedList = empty
    , orderedList = empty2
    , html =
        Markdown.Html.oneOf
            [ Markdown.Html.tag "section"
                (\id name renderedChildren ->
                    Html.a
                        ([ Attr.class "section" ]
                            ++ href PageNavigation ("#" ++ id)
                        )
                        [ Html.text name ]
                )
                |> Markdown.Html.withAttribute "id"
                |> Markdown.Html.withAttribute "name"
            , Markdown.Html.tag "div"
                (\id renderedChildren ->
                    Html.text ""
                )
                |> Markdown.Html.withAttribute "id"
            ]
    , codeBlock = empty
    , thematicBreak = empty0
    , table = empty
    , tableHeader = empty
    , tableBody = empty
    , tableRow = empty
    , tableHeaderCell = empty2
    , tableCell = empty
    }


{-| Converts nodes to virtual dom nodes.
-}
toVirtualDom : List Node -> List (Html msg)
toVirtualDom nodes =
    [ p []
        (List.map
            toVirtualDomEach
            nodes
        )
    ]


toVirtualDomEach : Node -> Html msg
toVirtualDomEach node =
    case node of
        Element "code" attrs [ Text s ] ->
            highlight s

        Element "span" [ ( "class", "inline-code" ) ] [ Text s ] ->
            Html.node "span" (List.map toAttribute [ ( "class", "inline-code" ) ]) [ text s ]

        Element name attrs children ->
            Html.node name (List.map toAttribute attrs) (toVirtualDom children)

        Text s ->
            let
                ( className, sanitized ) =
                    if String.endsWith "[Optional]" s then
                        ( "optional", String.replace "[Optional]" "" s )

                    else
                        ( "", s )
            in
            p [ CompoundHtmlAttributes.class className ] [ text sanitized ]

        Comment _ ->
            text ""


toAttribute : ( String, String ) -> Attribute msg
toAttribute ( name, value ) =
    Attr.attribute name value
