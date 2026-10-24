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
<!-- Table of contents of a module (toc.typ): nested list of page links -->
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xsl:import href="nelson_typst_common.xslt"/>
  <xsl:output method="text" encoding="UTF-8" omit-xml-declaration="yes"/>

  <xsl:variable name="module" select="normalize-space(/help_summary/toc/module)"/>

  <xsl:template match="/help_summary">
    <xsl:text>#import "nelson_help.typ": *&#10;&#10;</xsl:text>
    <xsl:apply-templates select="toc"/>
  </xsl:template>

  <xsl:template match="toc">
    <xsl:apply-templates select="section[not(@toc_visibility='false')]" mode="section">
      <xsl:with-param name="indent" select="''"/>
    </xsl:apply-templates>
  </xsl:template>

  <xsl:template match="section" mode="section">
    <xsl:param name="indent"/>
    <xsl:value-of select="$indent"/>
    <xsl:text>- </xsl:text>
    <xsl:call-template name="escape-typst-text">
      <xsl:with-param name="text" select="normalize-space(@name)"/>
    </xsl:call-template>
    <xsl:text>&#10;</xsl:text>
    <xsl:apply-templates select="section[not(@toc_visibility='false')]" mode="section">
      <xsl:with-param name="indent" select="concat($indent, '  ')"/>
    </xsl:apply-templates>
    <xsl:apply-templates select="keyword">
      <xsl:with-param name="indent" select="concat($indent, '  ')"/>
    </xsl:apply-templates>
  </xsl:template>

  <!-- "block:" aliases are lookup-only entries (doc, nflow block help):
       they stay in the TOC XML and index.json but are never rendered. -->
  <xsl:template match="keyword[starts-with(@name, 'block:')]"/>

  <xsl:template match="keyword">
    <xsl:param name="indent"/>
    <xsl:value-of select="$indent"/>
    <xsl:text>- #nlink(</xsl:text>
    <xsl:call-template name="page-label">
      <xsl:with-param name="module" select="$module"/>
      <xsl:with-param name="page" select="substring(@link, 1, string-length(@link) - 4)"/>
    </xsl:call-template>
    <xsl:text>)[</xsl:text>
    <xsl:call-template name="escape-typst-text">
      <xsl:with-param name="text" select="normalize-space(@name)"/>
    </xsl:call-template>
    <xsl:text>]&#10;</xsl:text>
  </xsl:template>
</xsl:stylesheet>
