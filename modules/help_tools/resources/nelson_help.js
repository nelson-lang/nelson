/* nelson_help.js
   Combined client-side helper used by generated help pages.
   - Loads MathJax (try local tex-mml-chtml.js, fallback to CDN)
   - Loads highlight.js (try local highlight.pack.js, fallback to CDN)
   - Initializes highlighting when ready
   - open external links in new tab
   - copyExample(btn) used by example copy buttons
   - buildGitHubEditUrl used to fill #github-edit-link
   - other existing helpers
*/
(function () {
  "use strict";

  // --- Loader utilities ---
  function createAndAppendScript(src, attrs, onload, onerror) {
    try {
      var s = document.createElement("script");
      s.src = src;
      s.async = true;
      if (attrs) {
        for (var k in attrs) {
          if (Object.prototype.hasOwnProperty.call(attrs, k))
            s.setAttribute(k, attrs[k]);
        }
      }
      if (onload) s.onload = onload;
      if (onerror) s.onerror = onerror;
      document.head.appendChild(s);
      return s;
    } catch (e) {
      if (onerror) onerror();
      return null;
    }
  }

  // Inject MathJax v2 config block (same content as previously in XSLT)
  function injectMathJaxConfig() {
    try {
      var cfg = document.createElement("script");
      cfg.type = "text/x-mathjax-config";
      cfg.text =
        "MathJax.Hub.Config({" +
        "tex2jax: {" +
        "inlineMath: [['$','$'], ['\\\\(','\\\\)']]," +
        "displayMath: [['$$','$$'], ['\\\\[','\\\\]']]," +
        "processEscapes: true," +
        "processEnvironments: true" +
        "}," +
        '"HTML-CSS": {' +
        'availableFonts: ["TeX"], preferredFont: "TeX", webFont: "TeX", imageFont: null' +
        "}," +
        "showMathMenu: false" +
        "});";
      document.head.appendChild(cfg);
    } catch (e) {
      /* ignore */
    }
  }

  // Try to load local MathJax (tex-mml-chtml.js), fallback to CDN v2
  function loadMathJax() {
    try {
      if (window.MathJax) return; // already loaded
      injectMathJaxConfig();
      var local = "../tex-mml-chtml.js";
      var cdn =
        "https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.9/MathJax.js?config=TeX-AMS_HTML";
      createAndAppendScript(local, null, null, function () {
        // on error -> load CDN
        createAndAppendScript(cdn);
      });
    } catch (e) {
      /* ignore */
    }
  }

  // Try to load local highlight.js, fallback to CDN. When loaded, run highlight and also attach DOMContentLoaded safeguard.
  function loadHighlightJS() {
    try {
      if (window.hljs) {
        runHighlight();
        return;
      }
      var local = "highlight.pack.js";
      var cdn =
        "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.7.0/highlight.min.js";
      function onLoad() {
        runHighlight();
      }
      createAndAppendScript(local, null, onLoad, function () {
        createAndAppendScript(cdn, null, onLoad);
      });
    } catch (e) {
      /* ignore */
    }
  }

  function runHighlight() {
    try {
      if (window.hljs) {
        // If DOM is already ready, highlight now
        if (document.readyState === "loading") {
          // DOM still loading, wait for DOMContentLoaded
          document.addEventListener("DOMContentLoaded", function () {
            try {
              if (window.hljs && typeof hljs.highlightAll === "function") {
                hljs.highlightAll();
              }
            } catch (e) {
              /* ignore */
            }
          });
        } else {
          // DOM already ready, highlight immediately
          try {
            if (typeof hljs.highlightAll === "function") {
              hljs.highlightAll();
            } else if (typeof hljs.initHighlightingOnLoad === "function") {
              hljs.initHighlightingOnLoad();
            }
          } catch (e) {
            /* ignore */
          }
        }
      }
    } catch (e) {
      /* ignore */
    }
  }

  // --- existing helpers (external links, copyExample, highlight init, buildGitHubEditUrl, etc.) ---

  // Delegate click to open external links in a new window
  function onDocClick(e) {
    try {
      var a =
        typeof e.target.closest === "function"
          ? e.target.closest("a[href]")
          : null;
      if (!a) return;
      var href = a.getAttribute("href");
      if (/^https?:\/\//i.test(href)) {
        e.preventDefault();
        var w = window.open(href, "_blank", "noopener");
        if (w)
          try {
            w.focus();
          } catch (_) {}
      }
    } catch (_) {
      /*ignore*/
    }
  }
  document.addEventListener("click", onDocClick, true);

  // Expose copyExample globally (used via onclick attribute in generated HTML)
  window.copyExample = function (btn) {
    try {
      var codeBlock =
        btn &&
        btn.parentNode &&
        btn.parentNode.parentNode &&
        btn.parentNode.parentNode.querySelector("code");
      if (!codeBlock) return;
      var text = codeBlock.textContent || "";
      if (navigator.clipboard && navigator.clipboard.writeText) {
        navigator.clipboard.writeText(text).then(
          function () {
            btn.setAttribute("data-copied", "true");
            btn.setAttribute("aria-label", "Copied!");
            btn.title = "Copied!";
            setTimeout(function () {
              btn.removeAttribute("data-copied");
              btn.setAttribute("aria-label", "Copy");
              btn.title = "Copy";
            }, 1200);
          },
          function () {
            btn.setAttribute("aria-label", "Error");
            btn.title = "Error";
          },
        );
      } else {
        // Fallback: select and execCommand
        var ta = document.createElement("textarea");
        ta.value = text;
        document.body.appendChild(ta);
        ta.select();
        try {
          document.execCommand("copy");
          btn.setAttribute("data-copied", "true");
        } catch (e) {
          btn.setAttribute("aria-label", "Error");
        }
        document.body.removeChild(ta);
        setTimeout(function () {
          btn.removeAttribute("data-copied");
          btn.setAttribute("aria-label", "Copy");
          btn.title = "Copy";
        }, 1200);
      }
    } catch (_) {
      /*ignore*/
    }
  };

  // Build GitHub edit URL (best guess) used by pages
  function buildGitHubEditUrl(href) {
    try {
      var m = href.match(
        /\/help\/(v[^\/]+)\/([^\/]+)\/([^\/]+)\/([^\/]+)\.html$/,
      );
      var version, lang, moduleName, basename;
      if (m) {
        version = m[1];
        lang = m[2];
        moduleName = m[3];
        basename = m[4];
      } else {
        var url = href.split("?")[0].split("#")[0];
        var parts = url.split("/").filter(function (p) {
          return p.length > 0;
        });
        // default branch when no explicit version is found
        version = "master";
        // Look for a segment that matches either 'v1.2.3' or numeric start
        for (var i = 0; i < parts.length; i++) {
          var seg = parts[i];
          if (/^v?\d+(\.\d+)*$/i.test(seg)) {
            version = seg;
            break;
          }
        }

        if (parts.length >= 2) {
          moduleName = parts[parts.length - 2];
          basename = parts[parts.length - 1]
            .replace(/\.html?$/i, "")
            .replace(/\.md$/i, "");
        } else if (parts.length === 1) {
          moduleName = parts[0];
          basename = parts[0];
        } else {
          moduleName = "unknown_module";
          basename = "index";
        }
        lang = "en";
        for (var j = 0; j < parts.length; j++) {
          if (/^fr(_|$)/i.test(parts[j]) || /^fr-/i.test(parts[j])) {
            lang = "fr";
            break;
          }
          if (/^en(_|$)/i.test(parts[j]) || /^en-/i.test(parts[j])) {
            lang = "en";
            break;
          }
        }
      }
      var mdLang = /^fr/i.test(lang) ? "fr" : "en";
      if (!version) version = "master";
      // keep previous behavior (prefixing with v may have been used before)
      var verSegment = /^v/i.test(String(version))
        ? encodeURIComponent(version)
        : "v" + encodeURIComponent(version);
      return (
        "https://github.com/nelson-lang/nelson-gitbook/blob/" +
        verSegment +
        "/markdown/" +
        mdLang +
        "/" +
        encodeURIComponent(moduleName) +
        "/" +
        encodeURIComponent(basename) +
        ".md"
      );
    } catch (e) {
      return "https://github.com/nelson-lang/nelson-gitbook";
    }
  }
  window.buildGitHubEditUrl = buildGitHubEditUrl;

  // After DOM ready: highlight MATLAB code blocks and set github edit link
  document.addEventListener("DOMContentLoaded", function () {
    // general highlighting is handled by highlight.js loader (hljs.highlightAll)
    try {
      var link = document.getElementById("github-edit-link");
      if (link) link.href = buildGitHubEditUrl(window.location.href);
    } catch (_) {}
  });

  // Initialize loaders now (script is included in head)
  try {
    loadMathJax();
  } catch (_) {}
  try {
    loadHighlightJS();
  } catch (_) {}

  // End IIFE
})();
