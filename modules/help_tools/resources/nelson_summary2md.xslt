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

<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xsl:output method="text" encoding="UTF-8" omit-xml-declaration="yes"/>

  <xsl:template match="/help_summary">
    <xsl:apply-templates select="toc"/>
  </xsl:template>

  <xsl:template match="toc">
    <xsl:apply-templates select="section">
      <xsl:with-param name="level" select="'#'"/>
    </xsl:apply-templates>
  </xsl:template>

  <xsl:template match="section">
    <xsl:param name="level"/>
    <xsl:if test="not(@link)">
      <xsl:value-of select="$level"/>
      <xsl:text> </xsl:text>
      <xsl:value-of select="@name"/>
      <xsl:text>&#10;&#10;</xsl:text>

      <xsl:apply-templates select="chapter_description"/>

      <xsl:apply-templates select="section">
        <xsl:with-param name="level" select="concat($level, '#')"/>
      </xsl:apply-templates>

      <xsl:if test="keyword">
        <xsl:value-of select="concat($level, '#')"/>
        <xsl:text> Functions&#10;&#10;</xsl:text>
        <xsl:apply-templates select="keyword"/>
        <xsl:text>&#10;</xsl:text>
      </xsl:if>
    </xsl:if>
  </xsl:template>

  <xsl:template match="chapter_description">
    <xsl:apply-templates/>
    <xsl:text>&#10;&#10;</xsl:text>
  </xsl:template>

  <xsl:template match="chapter_description/p">
    <xsl:text>&#10;</xsl:text>
    <xsl:apply-templates/>
    <xsl:text>&#10;</xsl:text>
  </xsl:template>

  <!-- Chapter links use module-root paths, just like page description links. -->
  <xsl:template match="chapter_description//link">
    <xsl:variable name="target" select="normalize-space(@linkend)"/>
    <xsl:choose>
      <xsl:when test="string-length($target) &gt; 0">
        <xsl:variable name="module" select="substring-before(substring-after($target, '{'), '}')"/>
        <xsl:text>[</xsl:text><xsl:apply-templates/><xsl:text>](</xsl:text>
        <xsl:choose>
          <!-- href resolved by the help builder from the keyword index -->
          <xsl:when test="@href"><xsl:value-of select="@href"/></xsl:when>
          <xsl:otherwise>
            <xsl:if test="string-length($module) &gt; 0">
              <xsl:text>../</xsl:text><xsl:value-of select="$module"/><xsl:text>/</xsl:text>
            </xsl:if>
            <xsl:choose>
              <xsl:when test="contains($target, '}')"><xsl:value-of select="substring-after($target, '}')"/></xsl:when>
              <xsl:otherwise><xsl:value-of select="$target"/></xsl:otherwise>
            </xsl:choose>
            <xsl:text>.md</xsl:text>
          </xsl:otherwise>
        </xsl:choose>
        <xsl:text>)</xsl:text>
      </xsl:when>
      <xsl:otherwise><xsl:apply-templates/></xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template match="chapter_description//b">
    <xsl:text>**</xsl:text>
    <xsl:apply-templates/>
    <xsl:text>**</xsl:text>
  </xsl:template>

  <xsl:template match="chapter_description//i">
    <xsl:text>_</xsl:text>
    <xsl:apply-templates/>
    <xsl:text>_</xsl:text>
  </xsl:template>

  <!-- Support <table> tags, as a markdown table: the first row is the header, then one line per row -->
  <xsl:template match="chapter_description//table">
    <xsl:variable name="rows" select="thead/tr | tbody/tr | tfoot/tr | tr"/>
    <xsl:text>&#10;</xsl:text>
    <xsl:apply-templates select="$rows[1]" mode="header"/>
    <xsl:apply-templates select="$rows[position() > 1]"/>
    <xsl:text>&#10;</xsl:text>
  </xsl:template>

  <xsl:template match="chapter_description//tr" mode="header">
    <xsl:apply-templates select="."/>
    <xsl:text>|</xsl:text>
    <xsl:for-each select="th|td">
      <xsl:text> --- |</xsl:text>
    </xsl:for-each>
    <xsl:text>&#10;</xsl:text>
  </xsl:template>

  <xsl:template match="chapter_description//tr">
    <xsl:text>|</xsl:text>
    <xsl:for-each select="th|td">
      <xsl:text> </xsl:text>
      <xsl:apply-templates/>
      <xsl:text> |</xsl:text>
    </xsl:for-each>
    <xsl:text>&#10;</xsl:text>
  </xsl:template>

  <!-- A markdown row is a single line: fold the whitespace of the text found in a cell -->
  <xsl:template match="chapter_description//table//text()">
    <xsl:choose>
      <xsl:when test="normalize-space(.) = ''">
        <xsl:if test="preceding-sibling::node() and following-sibling::node()">
          <xsl:text> </xsl:text>
        </xsl:if>
      </xsl:when>
      <xsl:otherwise>
        <xsl:if test="preceding-sibling::node() and normalize-space(substring(., 1, 1)) = ''">
          <xsl:text> </xsl:text>
        </xsl:if>
        <xsl:value-of select="normalize-space(.)"/>
        <xsl:if test="following-sibling::node() and normalize-space(substring(., string-length(.))) = ''">
          <xsl:text> </xsl:text>
        </xsl:if>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <!-- "block:" aliases are lookup-only entries (doc, nflow block help):
       they stay in the TOC XML and index.json but are never rendered. -->
  <xsl:template match="keyword[starts-with(@name, 'block:')]"/>

  <xsl:template match="keyword">
    <xsl:text>- [</xsl:text>
    <xsl:value-of select="@name"/>
    <xsl:text>](</xsl:text>
    <xsl:value-of select="@link"/>
    <xsl:text>) - </xsl:text>
    <xsl:value-of select="@description"/>
    <xsl:text>&#10;</xsl:text>
  </xsl:template>
</xsl:stylesheet>
