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
<!--
  Templates shared by the Typst stylesheets (page, summary, table of
  contents): text escaping, string literals and page labels.
-->
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

  <!-- <module:page> label; <page> alone when no module is known.
       Page paths use "." instead of "/" since labels cannot contain "/". -->
  <xsl:template name="page-label">
    <xsl:param name="module"/>
    <xsl:param name="page"/>
    <xsl:text>&lt;</xsl:text>
    <xsl:if test="string-length($module) &gt; 0">
      <xsl:value-of select="$module"/>
      <xsl:text>:</xsl:text>
    </xsl:if>
    <xsl:value-of select="translate($page, '/', '.')"/>
    <xsl:text>&gt;</xsl:text>
  </xsl:template>

  <!-- #nlink(<label>)[text] for a <link linkend="..."> element.
       target_module / target_page are set by the link resolver when the
       target page is known; otherwise the linkend syntax is used:
       "fn", "${module}fn" or "{module}fn", with $module as default. -->
  <xsl:template name="link-reference">
    <xsl:param name="module"/>
    <xsl:variable name="linkend" select="normalize-space(@linkend)"/>
    <xsl:variable name="target-module">
      <xsl:choose>
        <xsl:when test="@target_module">
          <xsl:value-of select="@target_module"/>
        </xsl:when>
        <xsl:when test="contains($linkend, '{') and contains($linkend, '}')">
          <xsl:value-of select="substring-before(substring-after($linkend, '{'), '}')"/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:value-of select="$module"/>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <xsl:variable name="target-page">
      <xsl:choose>
        <xsl:when test="@target_page">
          <xsl:value-of select="@target_page"/>
        </xsl:when>
        <xsl:when test="contains($linkend, '}')">
          <xsl:value-of select="substring-after($linkend, '}')"/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:value-of select="$linkend"/>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <xsl:text>#nlink(</xsl:text>
    <xsl:call-template name="page-label">
      <xsl:with-param name="module" select="$target-module"/>
      <xsl:with-param name="page" select="$target-page"/>
    </xsl:call-template>
    <xsl:text>)[</xsl:text>
    <xsl:call-template name="escape-typst-text">
      <xsl:with-param name="text" select="normalize-space(.)"/>
    </xsl:call-template>
    <xsl:text>];</xsl:text>
  </xsl:template>

  <!-- Typst string literal -->
  <xsl:template name="typst-string">
    <xsl:param name="text"/>
    <xsl:variable name="escaped-backslash">
      <xsl:call-template name="replace-string">
        <xsl:with-param name="text" select="$text"/>
        <xsl:with-param name="search" select="'\'"/>
        <xsl:with-param name="replace" select="'\\'"/>
      </xsl:call-template>
    </xsl:variable>
    <xsl:variable name="escaped-quote">
      <xsl:call-template name="replace-string">
        <xsl:with-param name="text" select="$escaped-backslash"/>
        <xsl:with-param name="search" select="'&quot;'"/>
        <xsl:with-param name="replace" select="'\&quot;'"/>
      </xsl:call-template>
    </xsl:variable>
    <xsl:variable name="escaped-newline">
      <xsl:call-template name="replace-string">
        <xsl:with-param name="text" select="$escaped-quote"/>
        <xsl:with-param name="search" select="'&#10;'"/>
        <xsl:with-param name="replace" select="'\n'"/>
      </xsl:call-template>
    </xsl:variable>
    <xsl:text>"</xsl:text>
    <xsl:value-of select="$escaped-newline"/>
    <xsl:text>"</xsl:text>
  </xsl:template>

  <!-- Escape characters that Typst markup would interpret -->
  <xsl:template name="escape-typst-text">
    <xsl:param name="text"/>
    <xsl:call-template name="escape-chars">
      <xsl:with-param name="text" select="$text"/>
      <xsl:with-param name="chars" select="'#$*_`&lt;&gt;@[]~/='"/>
    </xsl:call-template>
  </xsl:template>

  <!-- Backslash first, then each special character, one pass per character -->
  <xsl:template name="escape-chars">
    <xsl:param name="text"/>
    <xsl:param name="chars"/>
    <xsl:param name="first" select="true()"/>
    <xsl:variable name="current">
      <xsl:choose>
        <xsl:when test="$first">
          <xsl:call-template name="replace-string">
            <xsl:with-param name="text" select="$text"/>
            <xsl:with-param name="search" select="'\'"/>
            <xsl:with-param name="replace" select="'\\'"/>
          </xsl:call-template>
        </xsl:when>
        <xsl:otherwise>
          <xsl:value-of select="$text"/>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <xsl:choose>
      <xsl:when test="string-length($chars) = 0">
        <xsl:value-of select="$current"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:variable name="char" select="substring($chars, 1, 1)"/>
        <xsl:variable name="replaced">
          <xsl:call-template name="replace-string">
            <xsl:with-param name="text" select="$current"/>
            <xsl:with-param name="search" select="$char"/>
            <xsl:with-param name="replace" select="concat('\', $char)"/>
          </xsl:call-template>
        </xsl:variable>
        <xsl:call-template name="escape-chars">
          <xsl:with-param name="text" select="$replaced"/>
          <xsl:with-param name="chars" select="substring($chars, 2)"/>
          <xsl:with-param name="first" select="false()"/>
        </xsl:call-template>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template name="replace-string">
    <xsl:param name="text"/>
    <xsl:param name="search"/>
    <xsl:param name="replace"/>
    <xsl:choose>
      <xsl:when test="contains($text, $search)">
        <xsl:value-of select="substring-before($text, $search)"/>
        <xsl:value-of select="$replace"/>
        <xsl:call-template name="replace-string">
          <xsl:with-param name="text" select="substring-after($text, $search)"/>
          <xsl:with-param name="search" select="$search"/>
          <xsl:with-param name="replace" select="$replace"/>
        </xsl:call-template>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="$text"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

</xsl:stylesheet>
