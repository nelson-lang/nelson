/* nelson_help.js
   Combined client-side helper used by generated help pages.
   Provides:
   - open external links in new tab
   - copyExample(btn) used by example copy buttons
   - syntax highlighting initialization
   - best-guess GitHub edit URL builder and assignment for #github-edit-link
*/
(function () {
  "use strict";

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

  // Syntax highlighting initialization
  try {
    if (window.hljs && typeof hljs.highlightAll === "function")
      hljs.highlightAll();
  } catch (_) {}

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
        // Look for a segment that matches either 'v1.2.3' or '1.2.3' (numeric start)
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
      return (
        "https://github.com/nelson-lang/nelson-gitbook/blob/v" +
        encodeURIComponent(version) +
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
    try {
      document.querySelectorAll("code.matlab").forEach(function (block) {
        if (window.hljs) hljs.highlightElement(block);
      });
    } catch (_) {}
    try {
      var link = document.getElementById("github-edit-link");
      if (link) link.href = buildGitHubEditUrl(window.location.href);
    } catch (_) {}
  });
})();
