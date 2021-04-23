(function() {
  "use strict";

  let requested = false;

  const highlightTopic = (sections, navLinks, className) => {
    if (sections.length > 0) {
      let firstSection;

      if (window.innerHeight + window.scrollY >= document.body.offsetHeight) {
        firstSection = sections[0];
      } else {
        firstSection =
          sections.find(section => section.getBoundingClientRect().top < 69) ||
          sections[sections.length - 1];
      }

      navLinks.forEach(link => {
        if (link.hash === `#${firstSection.id}`) {
          link.classList.add(className);
        } else {
          link.classList.remove(className);
        }
      });
    }
  };

  const wrapAnimationFrame = fn => {
    if (!requested) {
      requested = true;

      window.requestAnimationFrame(() => {
        requested = false;
        fn();
      });
    }
  };

  let navlinksHandler;

  window.highlightScroll = (
    navLinkSelector,
    sectionSelector,
    className = "active"
  ) => {
    const navLinks = Array.from(document.querySelectorAll(navLinkSelector));
    const sections = Array.from(
      document.querySelectorAll(sectionSelector)
    ).reverse();
    const fn = highlightTopic.bind(null, sections, navLinks, className);

    if (navlinksHandler) {
      // Remove old listener, if exists
      window.removeEventListener("scroll", navlinksHandler);
    }

    navlinksHandler = event => {
      wrapAnimationFrame(fn);
    };

    window.addEventListener("scroll", navlinksHandler);

    navlinksHandler();
  };

  const processScrollOrResize = () => {
    const header = document.getElementById("header");
    const footer = document.getElementById("footer");
    const top = header !== null ? header.offsetHeight : 0;
    const bottom = footer !== null ? footer.offsetTop : 0;
    const element = document.getElementById("docs__main-navigation");
    const sideElement = document.getElementById("docs__side-navigation");
    let elementHeight = 0;

    if (element) {
      elementHeight = element.offsetHeight;
      if (window.pageYOffset > top) {
        element.classList.add("fixed");
      } else {
        element.classList.remove("fixed");
      }
    }

    if (sideElement) {
      const [contentElement] = sideElement.childNodes;
      if (
        window.pageYOffset + contentElement.offsetHeight + elementHeight >
        bottom
      ) {
        sideElement.classList.add("fixed--bottom");
        sideElement.classList.remove("fixed");
      } else if (window.pageYOffset >= top) {
        sideElement.classList.add("fixed");
        sideElement.classList.remove("fixed--bottom");
      } else {
        sideElement.classList.remove("fixed");
        sideElement.classList.remove("fixed--bottom");
      }
    }
  };

  window.addEventListener("scroll", processScrollOrResize);
  window.addEventListener("resize", processScrollOrResize);
})();
