<?xml version="1.0" encoding="UTF-8"?>
<!-- 
%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
-->
<!-- left part viewer -->
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xsl:output method="html" encoding="UTF-8" indent="yes" doctype-public="-//W3C//DTD HTML 4.01//EN"/>

  <xsl:template match="main_title|subtitle|version|brief_description"/>

  <xsl:template match="/help_summary">
    <html>
      <xsl:attribute name="lang">
        <xsl:choose>
          <xsl:when test="contains(xmldoc/language, '_')">
            <xsl:value-of select="substring-before(xmldoc/language, '_')"/>
          </xsl:when>
          <xsl:when test="contains(xmldoc/language, '-')">
            <xsl:value-of select="substring-before(xmldoc/language, '-')"/>
          </xsl:when>
          <xsl:when test="normalize-space(xmldoc/language)">
            <xsl:value-of select="xmldoc/language"/>
          </xsl:when>
          <xsl:otherwise>en</xsl:otherwise>
        </xsl:choose>
      </xsl:attribute>
      <head>
        <title>Nelson Table of Contents</title>
        <link rel="stylesheet" href="nelson_common.css"/>
        <script type="text/javascript">
          function toggleTocList(titleElem) {
            var arrow = titleElem.querySelector('.arrow');
            var ul = titleElem.parentNode.querySelector('ul.toc-list');
            if (!ul) return;
            var isVisible = ul.style.display !== 'none';
            if (isVisible) {
              ul.style.display = 'none';
              if (arrow) { arrow.innerHTML = '&#x25B8;'; }
            } else {
              ul.style.display = 'block';
              if (arrow) { arrow.innerHTML = '&#x25BE;'; }
            }
          }

          // Normalize a path so it can be matched against a toc-link href:
          // decode percent-encoding, drop query/hash and a leading "./" or "/".
          function nlsNormalizeTocPath(p) {
            if (!p) { return ''; }
            var s = String(p);
            try { s = decodeURIComponent(s); } catch (e) { /* keep raw */ }
            s = s.split('?')[0].split('#')[0];
            s = s.replace(/^\.?\//, '');
            return s.toLowerCase();
          }

          // Expand ancestors, highlight and scroll to the entry for pagePath.
          function nlsActivateTocEntry(pagePath) {
            var target = nlsNormalizeTocPath(pagePath);
            if (!target) { return; }
            var links = document.querySelectorAll('a.toc-link');
            var match = null;
            for (var i = 0; i &lt; links.length; i++) {
              var href = nlsNormalizeTocPath(links[i].getAttribute('href'));
              if (!href) { continue; }
              if (href === target ||
                  href.slice(-(target.length + 1)) === ('/' + target) ||
                  target.slice(-(href.length + 1)) === ('/' + href)) {
                match = links[i];
                break;
              }
            }
            var actives = document.querySelectorAll('a.toc-link.active');
            for (var j = 0; j &lt; actives.length; j++) { actives[j].classList.remove('active'); }
            if (!match) { nlsPostNeighbors(null, null); return; }
            match.classList.add('active');
            var el = match.parentNode;
            while (el &amp;&amp; el !== document.body) {
              if (el.tagName === 'UL' &amp;&amp;
                  (' ' + el.className + ' ').indexOf(' toc-list ') !== -1) {
                el.style.display = 'block';
                var title = el.previousElementSibling;
                if (title &amp;&amp; title.className &amp;&amp;
                    (' ' + title.className + ' ').indexOf(' section-title ') !== -1) {
                  var arrow = title.querySelector('.arrow');
                  if (arrow) { arrow.innerHTML = '&#x25BE;'; }
                }
              }
              el = el.parentNode;
            }
            try { match.scrollIntoView({ block: 'center' }); }
            catch (e) { match.scrollIntoView(); }
            nlsComputeAndPostNeighbors(match);
          }

          // Build a {href,label} descriptor for a toc link (or null).
          function nlsNeighbor(a) {
            if (!a) { return null; }
            return { href: a.getAttribute('href'), label: (a.textContent || '').replace(/^\s+|\s+$/g, '') };
          }
          function nlsPostNeighbors(prev, next) {
            try {
              if (window.parent &amp;&amp; window.parent !== window) {
                window.parent.postMessage(
                  { type: 'nelson-toc-neighbors', prev: nlsNeighbor(prev), next: nlsNeighbor(next) }, '*');
              }
            } catch (e) { /* ignore */ }
          }
          // Previous/next sibling functions = leaf links in the same toc-list.
          function nlsComputeAndPostNeighbors(match) {
            var li = match.parentNode;
            var ul = li ? li.parentNode : null;
            if (!ul || ul.tagName !== 'UL') { nlsPostNeighbors(null, null); return; }
            var sibs = [];
            var kids = ul.children;
            for (var k = 0; k &lt; kids.length; k++) {
              if (kids[k].tagName !== 'LI') { continue; }
              var a = null, cc = kids[k].children;
              for (var c = 0; c &lt; cc.length; c++) {
                if (cc[c].tagName === 'A' &amp;&amp;
                    (' ' + cc[c].className + ' ').indexOf(' toc-link ') !== -1) { a = cc[c]; break; }
              }
              if (a) { sibs.push(a); }
            }
            var idx = sibs.indexOf(match);
            var prev = idx &gt; 0 ? sibs[idx - 1] : null;
            var next = (idx !== -1 &amp;&amp; idx &lt; sibs.length - 1) ? sibs[idx + 1] : null;
            nlsPostNeighbors(prev, next);
          }

          // ---- In-TOC filter -----------------------------------------------------
          function nlsTocSetOpen(ul, open) {
            ul.style.display = open ? 'block' : 'none';
            var title = ul.previousElementSibling;
            if (title &amp;&amp; title.className &amp;&amp;
                (' ' + title.className + ' ').indexOf(' section-title ') !== -1) {
              var arrow = title.querySelector('.arrow');
              if (arrow) { arrow.innerHTML = open ? '&#x25BE;' : '&#x25B8;'; }
            }
          }
          function nlsFilterToc(raw) {
            var q = (raw || '').replace(/^\s+|\s+$/g, '').toLowerCase();
            var i;
            var lis = document.querySelectorAll('li.toc-entry');
            var topSections = [];
            var bodyKids = document.body.children;
            for (i = 0; i &lt; bodyKids.length; i++) {
              if (bodyKids[i].tagName === 'DIV' &amp;&amp;
                  (' ' + bodyKids[i].className + ' ').indexOf(' section ') !== -1) {
                topSections.push(bodyKids[i]);
              }
            }
            var uls = document.querySelectorAll('ul.toc-list');
            if (!q) {
              for (i = 0; i &lt; lis.length; i++) { lis[i].style.display = ''; }
              for (i = 0; i &lt; topSections.length; i++) { topSections[i].style.display = ''; }
              for (i = 0; i &lt; uls.length; i++) { nlsTocSetOpen(uls[i], false); }
              var act = document.querySelector('a.toc-link.active');
              if (act) {
                var e2 = act.parentNode;
                while (e2 &amp;&amp; e2 !== document.body) {
                  if (e2.tagName === 'UL' &amp;&amp;
                      (' ' + e2.className + ' ').indexOf(' toc-list ') !== -1) { nlsTocSetOpen(e2, true); }
                  e2 = e2.parentNode;
                }
              }
              return;
            }
            for (i = 0; i &lt; lis.length; i++) { lis[i].style.display = 'none'; }
            for (i = 0; i &lt; topSections.length; i++) { topSections[i].style.display = 'none'; }
            var links = document.querySelectorAll('a.toc-link');
            for (i = 0; i &lt; links.length; i++) {
              if ((links[i].textContent || '').toLowerCase().indexOf(q) === -1) { continue; }
              var el = links[i];
              while (el &amp;&amp; el !== document.body) {
                if (el.tagName === 'LI' &amp;&amp;
                    (' ' + el.className + ' ').indexOf(' toc-entry ') !== -1) { el.style.display = ''; }
                if (el.tagName === 'DIV' &amp;&amp; el.parentNode === document.body &amp;&amp;
                    (' ' + el.className + ' ').indexOf(' section ') !== -1) { el.style.display = ''; }
                if (el.tagName === 'UL' &amp;&amp;
                    (' ' + el.className + ' ').indexOf(' toc-list ') !== -1) { nlsTocSetOpen(el, true); }
                el = el.parentNode;
              }
            }
          }

          function nlsActivateFromHash() {
            if (window.location.hash &amp;&amp; window.location.hash.length &gt; 1) {
              var params = new URLSearchParams(window.location.hash.substring(1));
              var active = params.get('active');
              if (active) { nlsActivateTocEntry(active); }
            }
          }

          window.addEventListener('message', function (ev) {
            var d = ev &amp;&amp; ev.data;
            if (d &amp;&amp; d.type === 'nelson-toc-activate' &amp;&amp; d.page) {
              nlsActivateTocEntry(d.page);
            }
          });
          window.addEventListener('hashchange', nlsActivateFromHash);
          window.addEventListener('DOMContentLoaded', function () {
            var filter = document.getElementById('toc-filter');
            var clear = document.getElementById('toc-filter-clear');
            if (filter) {
              filter.addEventListener('input', function () {
                nlsFilterToc(filter.value);
                if (clear) { clear.hidden = !filter.value; }
              });
              filter.addEventListener('keydown', function (e) {
                if (e.key === 'Escape') { filter.value = ''; nlsFilterToc(''); if (clear) { clear.hidden = true; } }
              });
            }
            if (clear) {
              clear.addEventListener('click', function () {
                filter.value = ''; nlsFilterToc(''); clear.hidden = true; filter.focus();
              });
            }
            nlsActivateFromHash();
            if (window.parent &amp;&amp; window.parent !== window) {
              window.parent.postMessage({ type: 'nelson-toc-ready' }, '*');
            }
          });
        </script>
      </head>
      <body>
        <img src="banner_nelson_small.png" alt="Nelson banner" style="display:block;margin:16px auto;max-width:50%;height:auto;" onerror="this.style.display='none';"/>
        <div class="toc-filter-wrap">
          <input type="text" id="toc-filter" class="toc-filter-input" autocomplete="off" spellcheck="false">
            <xsl:attribute name="placeholder">
              <xsl:choose>
                <xsl:when test="contains(xmldoc/language, 'fr')">Filtrer les fonctions…</xsl:when>
                <xsl:otherwise>Filter functions…</xsl:otherwise>
              </xsl:choose>
            </xsl:attribute>
            <xsl:attribute name="aria-label">
              <xsl:choose>
                <xsl:when test="contains(xmldoc/language, 'fr')">Filtrer les fonctions</xsl:when>
                <xsl:otherwise>Filter functions</xsl:otherwise>
              </xsl:choose>
            </xsl:attribute>
          </input>
          <button type="button" id="toc-filter-clear" class="toc-filter-clear" title="Clear" aria-label="Clear filter" hidden="hidden">&#215;</button>
        </div>
        <xsl:apply-templates select="toc/overview"/>
        <xsl:apply-templates select="toc/section"/>
      </body>
    </html>
  </xsl:template>

  <xsl:template match="section">
    <xsl:if test="not(@toc_visibility) or @toc_visibility != 'false'">
      <xsl:choose>
        <xsl:when test="@link">
          <div class="section" style="margin-bottom: 0.1em;">
            <a class="toc-link" href="{@link}" target="contentFrame">
              <xsl:value-of select="@name"/>
            </a>
          </div>
        </xsl:when>
        <xsl:otherwise>
          <div class="section">
            <div class="section-title clickable" style="font-size: 1em;" onclick="toggleTocList(this)">
              <span class="arrow">&#x25B8;</span>
              <xsl:value-of select="@name"/>
            </div>
            <ul class="toc-list" style="display:none;">
              <xsl:apply-templates select="section" mode="nested"/>
              <xsl:apply-templates select="keyword"/>
            </ul>
          </div>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:if>
  </xsl:template>

  <xsl:template match="section" mode="nested">
    <xsl:if test="not(@toc_visibility) or @toc_visibility != 'false'">
      <li class="toc-entry">
        <xsl:choose>
          <xsl:when test="@link">
            <a class="toc-link" href="{@link}" target="contentFrame">
              <xsl:value-of select="@name"/>
            </a>
          </xsl:when>
          <xsl:otherwise>
            <div class="section-title clickable" style="font-size: 0.95em;" onclick="toggleTocList(this)">
              <span class="arrow">&#x25B8;</span>
              <xsl:value-of select="@name"/>
            </div>
            <ul class="toc-list" style="display:none;">
              <xsl:apply-templates select="section" mode="nested"/>
              <xsl:apply-templates select="keyword"/>
            </ul>
          </xsl:otherwise>
        </xsl:choose>
      </li>
    </xsl:if>
  </xsl:template>

  <!-- "block:" aliases are lookup-only entries (doc, nflow block help):
       they stay in the TOC XML and index.json but are never rendered. -->
  <xsl:template match="keyword[starts-with(@name, 'block:')]"/>

  <xsl:template match="keyword">
    <li class="toc-entry">
      <a class="toc-link" href="{ @link }" target="contentFrame"><xsl:value-of select="@name"/></a>
    </li>
  </xsl:template>
</xsl:stylesheet>
