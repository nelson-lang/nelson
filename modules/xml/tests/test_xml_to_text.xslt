<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
    
    <!-- Output method set to text -->
    <xsl:output method="text" encoding="UTF-8" indent="no"/>
    
    <!-- Root template -->
    <xsl:template match="/">
        <xsl:text>TABLE DATA REPORT&#10;</xsl:text>
        <xsl:text>===================&#10;&#10;</xsl:text>
        
        <!-- Header -->
        <xsl:text>Gender | Age    | State | Active&#10;</xsl:text>
        <xsl:text>-------|--------|-------|-------&#10;</xsl:text>
        
        <!-- Process each row -->
        <xsl:for-each select="table/row">
            <xsl:call-template name="format-row"/>
        </xsl:for-each>
        
        <!-- Summary -->
        <xsl:text>&#10;SUMMARY:&#10;</xsl:text>
        <xsl:text>--------&#10;</xsl:text>
        <xsl:text>Total records: </xsl:text>
        <xsl:value-of select="count(table/row)"/>
        <xsl:text>&#10;</xsl:text>
        
        <xsl:text>Male records: </xsl:text>
        <xsl:value-of select="count(table/row[Var1='M'])"/>
        <xsl:text>&#10;</xsl:text>
        
        <xsl:text>Female records: </xsl:text>
        <xsl:value-of select="count(table/row[Var1='F'])"/>
        <xsl:text>&#10;</xsl:text>
        
        <xsl:text>Active records: </xsl:text>
        <xsl:value-of select="count(table/row[Var4='true'])"/>
        <xsl:text>&#10;</xsl:text>
    </xsl:template>
    
    <!-- Template to format each row -->
    <xsl:template name="format-row">
        <!-- Gender (Var1) -->
        <xsl:choose>
            <xsl:when test="Var1='M'">
                <xsl:text>Male   </xsl:text>
            </xsl:when>
            <xsl:otherwise>
                <xsl:text>Female </xsl:text>
            </xsl:otherwise>
        </xsl:choose>
        
        <xsl:text>| </xsl:text>
        
        <!-- Age (first Var2) - pad to 6 characters -->
        <xsl:variable name="age" select="Var2[1]"/>
        <xsl:value-of select="$age"/>
        <xsl:call-template name="pad-spaces">
            <xsl:with-param name="count" select="6 - string-length($age)"/>
        </xsl:call-template>
        
        <xsl:text> | </xsl:text>
        
        <!-- State (Var3) -->
        <xsl:value-of select="Var3"/>
        <xsl:text>    | </xsl:text>
        
        <!-- Active status (Var4) -->
        <xsl:choose>
            <xsl:when test="Var4='true'">
                <xsl:text>Yes</xsl:text>
            </xsl:when>
            <xsl:otherwise>
                <xsl:text>No</xsl:text>
            </xsl:otherwise>
        </xsl:choose>
        
        <!-- Handle multiple Var2 values if present -->
        <xsl:if test="count(Var2) &gt; 1">
            <xsl:text> (Alt: </xsl:text>
            <xsl:for-each select="Var2[position() &gt; 1]">
                <xsl:value-of select="."/>
                <xsl:if test="position() != last()">
                    <xsl:text>, </xsl:text>
                </xsl:if>
            </xsl:for-each>
            <xsl:text>)</xsl:text>
        </xsl:if>
        
        <xsl:text>&#10;</xsl:text>
    </xsl:template>
    
    <!-- Helper template to add padding spaces -->
    <xsl:template name="pad-spaces">
        <xsl:param name="count"/>
        <xsl:if test="$count &gt; 0">
            <xsl:text> </xsl:text>
            <xsl:call-template name="pad-spaces">
                <xsl:with-param name="count" select="$count - 1"/>
            </xsl:call-template>
        </xsl:if>
    </xsl:template>
    
</xsl:stylesheet>