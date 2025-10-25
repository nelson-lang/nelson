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
    
    <!-- Main template -->
    <xsl:template match="/help_summary">
        <xsl:apply-templates select="toc"/>
    </xsl:template>
    
    <!-- TOC template -->
    <xsl:template match="toc">
        <!-- Output module as first item -->
        <xsl:text>- [</xsl:text>
        <xsl:value-of select="module"/>
        <xsl:text>](README.md)&#10;</xsl:text>
        
        <!-- Output sections with link as top-level links -->
        <xsl:for-each select="section[@link and not(@toc_visibility='false')]">
            <xsl:text>- [</xsl:text>
            <xsl:value-of select="@name"/>
            <xsl:text>](</xsl:text>
            <xsl:value-of select="@link"/>
            <xsl:text>)&#10;</xsl:text>
        </xsl:for-each>
        
        <!-- Output keywords for sections without link -->
        <xsl:apply-templates select="section[not(@link) and not(@toc_visibility='false')]/keyword"/>
    </xsl:template>
    
    <!-- Keyword template -->
    <xsl:template match="keyword">
        <xsl:text>&#32;&#32;- [</xsl:text>
        <xsl:value-of select="@name"/>
        <xsl:text>](</xsl:text>
        <xsl:value-of select="@link"/>
        <xsl:text>)&#10;</xsl:text>
    </xsl:template>
   
   
</xsl:stylesheet>