module CompoundComponents.Utils.Markdown.TableParser exposing (parser)

import CompoundComponents.Utils.Markdown.Table as MarkdownTable
import Parser
import Parser.Advanced as Advanced exposing (..)


type alias Parser a =
    Advanced.Parser String Parser.Problem a


type alias Table =
    MarkdownTable.Table String



-- taken from: https://github.com/stephenreddek/elm-markdown/blob/gfm-tables/src/Markdown/TableParser.elm


parser : Parser Table
parser =
    (Advanced.succeed (\headers delimiters body -> ( headers, delimiters, body ))
        |= rowParser
        |= delimiterRowParser
        |= bodyParser
    )
        |> Advanced.andThen
            (\( headers, DelimiterRow delimiterCount, body ) ->
                if List.length headers == delimiterCount then
                    Advanced.succeed
                        (MarkdownTable.Table
                            (headers
                                |> List.map
                                    (\headerCell ->
                                        { label = headerCell
                                        , alignment = Nothing
                                        }
                                    )
                            )
                            body
                        )

                else
                    Advanced.problem (Parser.Problem "Tables must have the same number of header columns as delimiter columns")
            )


rowParser : Parser (List String)
rowParser =
    succeed
        (\rowString ->
            rowString
                |> dropTrailingPipe
                |> String.split "|"
                |> List.map String.trim
        )
        |. oneOf
            [ tokenHelp "|"
            , succeed ()
            ]
        |= Advanced.getChompedString
            (Advanced.chompUntilEndOr "\n")
        |. oneOf
            [ Advanced.end (Parser.Expecting "end")
            , chompIf (\c -> c == '\n') (Parser.Expecting "\\n")
            ]


type DelimiterRow
    = DelimiterRow Int


delimiterRowParser : Parser DelimiterRow
delimiterRowParser =
    loop 0 delimiterRowHelp
        |> andThen
            (\((DelimiterRow count) as delimiterRow) ->
                if count > 0 then
                    succeed delimiterRow

                else
                    problem (Parser.Expecting "Must have at least one column in delimiter row.")
            )


requirePipeIfNotFirst : Int -> Parser ()
requirePipeIfNotFirst found =
    if found > 0 then
        tokenHelp "|"

    else
        oneOf
            [ tokenHelp "|"
            , succeed ()
            ]


delimiterRowHelp : Int -> Parser (Step Int DelimiterRow)
delimiterRowHelp found =
    oneOf
        [ tokenHelp "|\n" |> Advanced.map (\_ -> Done (DelimiterRow found))
        , tokenHelp "\n" |> Advanced.map (\_ -> Done (DelimiterRow found))
        , Advanced.end (Parser.Expecting "end") |> Advanced.map (\_ -> Done (DelimiterRow found))
        , backtrackable (succeed (Done (DelimiterRow found)) |. tokenHelp "|" |. Advanced.end (Parser.Expecting "end"))
        , succeed (Loop (found + 1))
            |. requirePipeIfNotFirst found
            |. spaces
            |. oneOrMore (\c -> c == '-')
            |. spaces
        ]


bodyParser : Parser (List (List String))
bodyParser =
    loop [] bodyParserHelp


bodyParserHelp : List (List String) -> Parser (Step (List (List String)) (List (List String)))
bodyParserHelp revRows =
    oneOf
        [ rowParser
            |> andThen
                (\row ->
                    if List.isEmpty row || List.all String.isEmpty row then
                        Advanced.problem (Parser.Problem "A line must have at least one column")

                    else
                        Advanced.succeed (Loop (row :: revRows))
                )
        , tokenHelp "\n"
            |> map (\_ -> Done (List.reverse revRows))
        , Advanced.end (Parser.Expecting "end")
            |> map (\_ -> Done (List.reverse revRows))
        ]


dropTrailingPipe : String -> String
dropTrailingPipe string =
    if string |> String.endsWith "|" then
        string
            |> String.dropRight 1

    else
        string



-- taken from https://github.com/dillonkearns/elm-markdown/blob/master/src/Parser/Extra.elm


oneOrMore : (Char -> Bool) -> Advanced.Parser c Parser.Problem ()
oneOrMore condition =
    chompIf condition (Parser.Problem "Expected one or more character")
        |. chompWhile condition


zeroOrMore : (Char -> Bool) -> Advanced.Parser c x ()
zeroOrMore condition =
    chompWhile condition


positiveInteger : Advanced.Parser c Parser.Problem Int
positiveInteger =
    mapChompedString (\str _ -> String.toInt str |> Maybe.withDefault 0) <|
        oneOrMore Char.isDigit


tokenHelp : String -> Advanced.Parser c Parser.Problem ()
tokenHelp char =
    Advanced.token (Advanced.Token char (Parser.Expecting char))
