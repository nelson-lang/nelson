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

  <xsl:template match="keyword">
    <li class="toc-entry">
      <a class="toc-link" href="{ @link }" target="contentFrame"><xsl:value-of select="@name"/></a>
    </li>
  </xsl:template>
</xsl:stylesheet>
