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
    <xsl:text>- [</xsl:text>
    <xsl:value-of select="module"/>
    <xsl:text>](README.md)&#10;</xsl:text>

    <xsl:for-each select="section[@link and not(@toc_visibility='false')]">
      <xsl:text>- [</xsl:text>
      <xsl:value-of select="@name"/>
      <xsl:text>](</xsl:text>
      <xsl:value-of select="@link"/>
      <xsl:text>)&#10;</xsl:text>
    </xsl:for-each>

    <xsl:apply-templates select="section[not(@link) and not(@toc_visibility='false')]/section" mode="section">
      <xsl:with-param name="indent" select="'  '"/>
    </xsl:apply-templates>
    <xsl:apply-templates select="section[not(@link) and not(@toc_visibility='false')]/keyword">
      <xsl:with-param name="indent" select="'  '"/>
    </xsl:apply-templates>
  </xsl:template>

  <xsl:template match="section" mode="section">
    <xsl:param name="indent"/>
    <xsl:if test="not(@toc_visibility='false')">
      <xsl:value-of select="$indent"/>
      <xsl:text>- </xsl:text>
      <xsl:value-of select="@name"/>
      <xsl:text>&#10;</xsl:text>
      <xsl:apply-templates select="section" mode="section">
        <xsl:with-param name="indent" select="concat($indent, '  ')"/>
      </xsl:apply-templates>
      <xsl:apply-templates select="keyword">
        <xsl:with-param name="indent" select="concat($indent, '  ')"/>
      </xsl:apply-templates>
    </xsl:if>
  </xsl:template>

  <xsl:template match="keyword">
    <xsl:param name="indent"/>
    <xsl:value-of select="$indent"/>
    <xsl:text>- [</xsl:text>
    <xsl:value-of select="@name"/>
    <xsl:text>](</xsl:text>
    <xsl:value-of select="@link"/>
    <xsl:text>)&#10;</xsl:text>
  </xsl:template>
</xsl:stylesheet>
