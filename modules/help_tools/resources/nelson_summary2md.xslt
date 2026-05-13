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

  <xsl:template match="chapter_description/b">
    <xsl:text>**</xsl:text>
    <xsl:apply-templates/>
    <xsl:text>**</xsl:text>
  </xsl:template>

  <xsl:template match="chapter_description/i">
    <xsl:text>_</xsl:text>
    <xsl:apply-templates/>
    <xsl:text>_</xsl:text>
  </xsl:template>

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
