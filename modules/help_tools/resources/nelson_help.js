// %=============================================================================
// % Copyright (c) 2016-present Allan CORNET (Nelson)
// %=============================================================================
// % This file is part of Nelson.
// %=============================================================================
// % LICENCE_BLOCK_BEGIN
// % SPDX-License-Identifier: LGPL-3.0-or-later
// % LICENCE_BLOCK_END
// %=============================================================================

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

  // Inject MathJax v3 config via window.MathJax before loading the script
  function prepareMathJaxV3Config() {
    try {
      // If a MathJax v3 config is already present, do not override
      if (window.MathJax && window.MathJax.tex) return;
      window.MathJax = {
        tex: {
          inlineMath: [
            ["$", "$"],
            ["\\(", "\\)"],
          ],
          displayMath: [
            ["$$", "$$"],
            ["\\[", "\\]"],
          ],
          processEscapes: true,
          processEnvironments: true,
        },
        options: {
          // avoid processing inside <pre> / <code> etc.
          skipHtmlTags: ["script", "noscript", "style", "textarea", "pre"],
        },
      };
    } catch (e) {
      /* ignore */
    }
  }

  // Try to load local MathJax v3 (tex-mml-chtml.js), fallback to CDN v3
  function loadMathJax() {
    try {
      // If MathJax v3 already present, typeset now
      if (
        window.MathJax &&
        typeof window.MathJax.typesetPromise === "function"
      ) {
        window.MathJax.typesetPromise().catch(function () {
          /*ignore*/
        });
        // ensure math rendered (fallback if needed)
        ensureMathRendered();
        return;
      }
      // If MathJax v2 present, request a typeset using v2 API and return
      if (
        window.MathJax &&
        window.MathJax.Hub &&
        typeof window.MathJax.Hub.Queue === "function"
      ) {
        window.MathJax.Hub.Queue(["Typeset", window.MathJax.Hub]);
        // ensure math rendered (fallback if needed)
        ensureMathRendered();
        return;
      }

      prepareMathJaxV3Config();
      var local = "../tex-mml-chtml.js";

      var cdn = "https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js";

      function onLoad() {
        try {
          if (
            window.MathJax &&
            typeof window.MathJax.typesetPromise === "function"
          ) {
            window.MathJax.typesetPromise()
              .catch(function () {
                /*ignore*/
              })
              .then(function () {
                ensureMathRendered();
              });
          } else if (
            window.MathJax &&
            window.MathJax.Hub &&
            typeof window.MathJax.Hub.Queue === "function"
          ) {
            window.MathJax.Hub.Queue(["Typeset", window.MathJax.Hub]);
            // small delay then check
            setTimeout(ensureMathRendered, 50);
          } else {
            // in case MathJax not available, still try fallback after short delay
            setTimeout(ensureMathRendered, 50);
          }
        } catch (_) {
          /* ignore */
        }
      }

      // Try CDN first (avoids file:/// resolution of sub-resources), fall back to local if CDN fails.
      createAndAppendScript(
        cdn,
        { crossorigin: "anonymous" },
        onLoad,
        function () {
          // CDN failed -> try local copy
          createAndAppendScript(local, null, onLoad);
        },
      );
    } catch (e) {
      /* ignore */
    }
  }

  // Fallback: if MathJax did not render <script type="math/tex"> blocks,
  // replace them with $$...$$ text nodes so MathJax will typeset them.
  function ensureMathRendered() {
    try {
      // If MathJax v3 present, typeset now and return
      if (
        window.MathJax &&
        typeof window.MathJax.typesetPromise === "function"
      ) {
        // typesetPromise will render math in the page; call it anyway
        window.MathJax.typesetPromise().catch(function () {
          /*ignore*/
        });
        return;
      }
      // If MathJax v2 present, queue a typeset
      if (
        window.MathJax &&
        window.MathJax.Hub &&
        typeof window.MathJax.Hub.Queue === "function"
      ) {
        window.MathJax.Hub.Queue(["Typeset", window.MathJax.Hub]);
        return;
      }

      // No MathJax available yet: still attempt to convert <script type="math/tex"> to $$...$$
      var scripts = Array.prototype.slice.call(
        document.querySelectorAll('script[type^="math/tex"]'),
      );
      if (!scripts || scripts.length === 0) return;
      var replaced = false;
      scripts.forEach(function (s) {
        try {
          // skip if already replaced
          if (s.getAttribute && s.getAttribute("data-math-processed") === "1")
            return;
          var tex = s.textContent || s.innerText || "";
          if (!tex) return;
          var span = document.createElement("span");
          // display-mode math: use $$...$$
          span.textContent = "$$" + tex + "$$";
          s.parentNode.replaceChild(span, s);
          replaced = true;
        } catch (_) {
          /* ignore */
        }
      });
      // If we replaced content and MathJax loads later it will find $$...$$; try to typeset now too
      if (
        replaced &&
        window.MathJax &&
        typeof window.MathJax.typesetPromise === "function"
      ) {
        window.MathJax.typesetPromise().catch(function () {
          /*ignore*/
        });
      }
    } catch (_) {
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
        if (version.startsWith("v")) {
          // remove 'v' prefix for further processing
          version = version.substring(1);
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
    // run a short delayed ensure to handle math rendering fallback if needed
    setTimeout(ensureMathRendered, 100);
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
