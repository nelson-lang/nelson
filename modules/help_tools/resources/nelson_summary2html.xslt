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
<!-- right part viewer -->
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xsl:output method="html" encoding="UTF-8" indent="yes" doctype-public="-//W3C//DTD HTML 4.01//EN"/>

  <xsl:template match="/">
    <html lang="en">
      <head>
        <link rel="stylesheet" href="nelson_common.css"/>
      </head>
      <body>
        <img src="banner_nelson_small.png" alt="Nelson banner" style="display:block;margin:16px auto;max-width:50%;height:auto;" onerror="this.style.display='none';"/>
        <!-- Help Summary Button at top right -->
        <xsl:variable name="mainSummaryAttr" select="normalize-space(help_summary/main_summary/@link)"/>
        <xsl:variable name="homeLink">
          <xsl:choose>
            <xsl:when test="string-length($mainSummaryAttr) &gt; 0">
              <xsl:value-of select="$mainSummaryAttr"/>
            </xsl:when>
            <xsl:otherwise>
              <xsl:text>../summary.html</xsl:text>
            </xsl:otherwise>
          </xsl:choose>
        </xsl:variable>
        <a href="{$homeLink}" style="position:fixed;top:18px;right:18px;z-index:1000;text-decoration:none;">
          <button style="background:#1976d2;color:#fff;border:none;padding:8px 14px;font-size:1.3em;border-radius:50%;box-shadow:0 2px 8px rgba(0,0,0,0.07);cursor:pointer;transition:background 0.2s;display:flex;align-items:center;justify-content:center;width:44px;height:44px;">
            <svg xmlns="http://www.w3.org/2000/svg" width="22" height="22" viewBox="0 0 24 24" fill="none" stroke="white" stroke-width="2" stroke-linecap="round" stroke-linejoin="round">
              <path d="M3 12L12 3l9 9"/>
              <path d="M9 21V12h6v9"/>
              <path d="M9 21h6"/>
            </svg>
          </button>
        </a>
        <xsl:apply-templates select="help_summary/toc/overview"/>
        <xsl:apply-templates select="help_summary/toc/section"/>
      </body>
    </html>
  </xsl:template>

  <xsl:template match="overview">
    <div class="section">
      <div class="section-title">Overview</div>
      <p><a class="toc-link" href="{ @link }">Overview</a></p>
    </div>
  </xsl:template>

  <xsl:template match="section">
    <xsl:if test="not(@link)">
      <div class="section">
        <div class="section-title">
          <xsl:value-of select="@name"/>
        </div>
        <xsl:if test="@description">
          <div class="section-desc" style="font-size:0.95em;color:#666;margin-bottom:8px;margin-left:24px;">
            <xsl:value-of select="@description"/>
          </div>
        </xsl:if>
        <xsl:if test="chapter_description">
          <div class="chapter-desc" style="font-size:0.97em;color:#444;margin-bottom:10px;margin-left:24px;">
            <xsl:apply-templates select="chapter_description"/>
          </div>
        </xsl:if>
        <ul class="toc-list">
          <xsl:apply-templates select="keyword"/>
        </ul>
      </div>
    </xsl:if>
  </xsl:template>

  <xsl:template match="keyword">
    <li class="toc-entry">
      <a class="toc-link" href="{ @link }" target="contentFrame"><xsl:value-of select="@name"/></a>
      <xsl:if test="@description">
        <div class="keyword-desc" style="font-size:0.92em; color:#888; margin-left:18px; margin-top:2px;">
          <xsl:value-of select="@description"/>
        </div>
      </xsl:if>
    </li>
  </xsl:template>

  <!-- Add this template to handle chapter_description inner HTML -->
  <xsl:template match="chapter_description">
    <xsl:apply-templates/>
  </xsl:template>

  <!-- Support <p> tags -->
  <xsl:template match="chapter_description/p">
    <p>
      <xsl:apply-templates/>
    </p>
  </xsl:template>

  <!-- Support <b> tags -->
  <xsl:template match="chapter_description/b">
    <b>
      <xsl:apply-templates/>
    </b>
  </xsl:template>

  <!-- Support <i> tags -->
  <xsl:template match="chapter_description/i">
    <i>
      <xsl:apply-templates/>
    </i>
  </xsl:template>
</xsl:stylesheet>