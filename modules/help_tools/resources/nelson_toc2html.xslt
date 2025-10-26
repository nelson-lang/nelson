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

  <!-- Suppress output for these tags -->
  <xsl:template match="main_title|subtitle|version|brief_description"/>

  <xsl:template match="/help_summary">
        <html>
        <xsl:attribute name="lang">
            <xsl:choose>
                <!-- en_US or zh_CN style -->
                <xsl:when test="contains(xmldoc/language, '_')">
                    <xsl:value-of select="substring-before(xmldoc/language, '_')"/>
                </xsl:when>
                <!-- en-US style -->
                <xsl:when test="contains(xmldoc/language, '-')">
                    <xsl:value-of select="substring-before(xmldoc/language, '-')"/>
                </xsl:when>
                <!-- bare language code like "en" or "fr" -->
                <xsl:when test="normalize-space(xmldoc/language)">
                    <xsl:value-of select="xmldoc/language"/>
                </xsl:when>
                <!-- default -->
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
              if (arrow) { arrow.innerHTML = '▸'; }
            } else {
              ul.style.display = 'block';
              if (arrow) { arrow.innerHTML = '▾'; }
            }
          }
        </script>
      </head>
      <body>
        <img src="banner_nelson_small.png" alt="Nelson banner" style="display:block;margin:16px auto;max-width:50%;height:auto;" onerror="this.style.display='none';"/>
        <xsl:apply-templates select="toc/overview"/>
        <xsl:apply-templates select="toc/section"/>
      </body>
    </html>
  </xsl:template>

  <xsl:template match="section">
    <!-- Only render if toc_visibility attribute is not 'false' -->
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
            <xsl:choose>
              <xsl:when test="keyword">
                <div class="section-title clickable" style="font-size: 1em;" onclick="toggleTocList(this)">
                  <!-- start collapsed: arrow shows collapsed glyph, list hidden -->
                  <span class="arrow">▸</span>
                  <xsl:value-of select="@name"/>
                </div>
                <ul class="toc-list" style="display:none;">
                  <xsl:apply-templates select="keyword"/>
                </ul>
              </xsl:when>
              <xsl:otherwise>
                <div class="section-title" style="font-size: 1em;">
                  <span class="arrow" style="visibility:hidden;">&#x25BE;</span>
                  <xsl:value-of select="@name"/>
                </div>
              </xsl:otherwise>
            </xsl:choose>
          </div>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:if>
  </xsl:template>

  <xsl:template match="keyword">
    <li class="toc-entry">
      <a class="toc-link" href="{ @link }" target="contentFrame"><xsl:value-of select="@name"/></a>
    </li>
  </xsl:template>

</xsl:stylesheet>