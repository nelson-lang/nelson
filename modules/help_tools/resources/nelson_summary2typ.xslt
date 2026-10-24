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
  Module root document (main.typ): chapters with their descriptions and the
  list of functions, followed by every generated page included one heading
  level below. Compile it alone or include it from a larger document.
-->
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xsl:import href="nelson_typst_common.xslt"/>
  <xsl:output method="text" encoding="UTF-8" omit-xml-declaration="yes"/>

  <xsl:variable name="module" select="normalize-space(/help_summary/toc/module)"/>

  <xsl:template match="/help_summary">
    <xsl:text>#import "nelson_help.typ": *&#10;&#10;</xsl:text>
    <xsl:apply-templates select="toc/section">
      <xsl:with-param name="level" select="'='"/>
    </xsl:apply-templates>
    <xsl:text>&#10;#nested[&#10;</xsl:text>
    <!-- One include per page: aliases share the link of their page -->
    <xsl:for-each select="//keyword[not(starts-with(@name, 'block:')) and not(@link = preceding::keyword[not(starts-with(@name, 'block:'))]/@link)]">
      <xsl:text>#pagebreak(weak: true)&#10;#include "</xsl:text>
      <xsl:value-of select="@link"/>
      <xsl:text>"&#10;</xsl:text>
    </xsl:for-each>
    <xsl:text>]&#10;</xsl:text>
  </xsl:template>

  <xsl:template match="section">
    <xsl:param name="level"/>
    <xsl:if test="not(@link)">
      <xsl:value-of select="$level"/>
      <xsl:text> </xsl:text>
      <xsl:value-of select="@name"/>
      <xsl:text>&#10;&#10;</xsl:text>

      <xsl:apply-templates select="chapter_description"/>

      <xsl:if test="keyword[not(starts-with(@name, 'block:'))]">
        <xsl:value-of select="concat($level, '=')"/>
        <xsl:text> Functions&#10;&#10;</xsl:text>
        <xsl:apply-templates select="keyword"/>
        <xsl:text>&#10;</xsl:text>
      </xsl:if>

      <xsl:apply-templates select="section">
        <xsl:with-param name="level" select="concat($level, '=')"/>
      </xsl:apply-templates>
    </xsl:if>
  </xsl:template>

  <xsl:template match="chapter_description">
    <xsl:apply-templates/>
  </xsl:template>

  <xsl:template match="chapter_description/p">
    <xsl:apply-templates/>
    <xsl:text>&#10;&#10;</xsl:text>
  </xsl:template>

  <xsl:template match="chapter_description//link">
    <xsl:call-template name="link-reference">
      <xsl:with-param name="module" select="$module"/>
    </xsl:call-template>
  </xsl:template>

  <!-- Inline expressions end with ';' so that following text starting with
       '(' '[' or '.name' is never parsed by Typst as a call or field access. -->
  <xsl:template match="chapter_description//b">
    <xsl:text>#strong[</xsl:text>
    <xsl:apply-templates/>
    <xsl:text>];</xsl:text>
  </xsl:template>

  <xsl:template match="chapter_description//i">
    <xsl:text>#emph[</xsl:text>
    <xsl:apply-templates/>
    <xsl:text>];</xsl:text>
  </xsl:template>

  <xsl:template match="chapter_description//code">
    <xsl:text>#raw(</xsl:text>
    <xsl:call-template name="typst-string">
      <xsl:with-param name="text" select="."/>
    </xsl:call-template>
    <xsl:text>);</xsl:text>
  </xsl:template>

  <xsl:template match="chapter_description//table">
    <xsl:variable name="rows" select="thead/tr | tbody/tr | tfoot/tr | tr"/>
    <xsl:text>&#10;#table(&#10;  columns: </xsl:text>
    <xsl:value-of select="count($rows[1]/th | $rows[1]/td)"/>
    <xsl:text>,&#10;  table.header(</xsl:text>
    <xsl:apply-templates select="$rows[1]/th | $rows[1]/td"/>
    <xsl:text>),&#10;</xsl:text>
    <xsl:for-each select="$rows[position() > 1]">
      <xsl:text>  </xsl:text>
      <xsl:apply-templates select="th | td"/>
      <xsl:text>&#10;</xsl:text>
    </xsl:for-each>
    <xsl:text>)&#10;</xsl:text>
  </xsl:template>

  <xsl:template match="chapter_description//th | chapter_description//td">
    <xsl:text>[</xsl:text>
    <xsl:apply-templates/>
    <xsl:text>], </xsl:text>
  </xsl:template>

  <xsl:template match="chapter_description//text()">
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
        <xsl:call-template name="escape-typst-text">
          <xsl:with-param name="text" select="normalize-space(.)"/>
        </xsl:call-template>
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
    <xsl:text>- #nlink(</xsl:text>
    <xsl:call-template name="page-label">
      <xsl:with-param name="module" select="$module"/>
      <xsl:with-param name="page" select="substring(@link, 1, string-length(@link) - 4)"/>
    </xsl:call-template>
    <xsl:text>)[</xsl:text>
    <xsl:call-template name="escape-typst-text">
      <xsl:with-param name="text" select="@name"/>
    </xsl:call-template>
    <xsl:text>]: </xsl:text>
    <xsl:call-template name="escape-typst-text">
      <xsl:with-param name="text" select="normalize-space(@description)"/>
    </xsl:call-template>
    <xsl:text>&#10;</xsl:text>
  </xsl:template>

</xsl:stylesheet>
