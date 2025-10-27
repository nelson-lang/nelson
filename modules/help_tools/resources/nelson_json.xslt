<?xml version="1.0" encoding="UTF-8"?>
<!--
  Generated from nelson_markdown.xslt / nelson_html.xslt (adapted for JSON output)
  Produces a compact JSON object with fields:
    "keyword", "Syntax", "short_description", "Input Arguments",
    "Output Arguments", "description", "see also"
-->
<xsl:stylesheet version="1.0"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:ext="http://io.github.nelson_lang/ext">

  <xsl:output method="text" encoding="UTF-8"/>

  <!-- Simple recursive replace template (text replacement) -->
  <xsl:template name="replace">
    <xsl:param name="text"/>
    <xsl:param name="search"/>
    <xsl:param name="replace"/>
    <xsl:choose>
      <!-- Guard: only attempt replacement when search is non-empty and present in text -->
      <xsl:when test="string-length($search) &gt; 0 and contains($text, $search)">
        <xsl:variable name="before" select="substring-before($text, $search)"/>
        <xsl:variable name="after" select="substring-after($text, $search)"/>
        <!-- If substring-after didn't make progress (shouldn't happen unless search is empty), bail out to avoid infinite loop -->
        <xsl:choose>
          <xsl:when test="$after = $text">
            <xsl:value-of select="$text"/>
          </xsl:when>
          <xsl:otherwise>
            <xsl:value-of select="$before"/>
            <xsl:value-of select="$replace"/>
            <xsl:call-template name="replace">
              <xsl:with-param name="text" select="$after"/>
              <xsl:with-param name="search" select="$search"/>
              <xsl:with-param name="replace" select="$replace"/>
            </xsl:call-template>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="$text"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <!-- escape JSON-special characters: backslash, double-quote, newline, carriage return, tab -->
  <xsl:template name="escape-json">
    <xsl:param name="text"/>
    <!-- step 1: backslash -> \\ -->
    <xsl:variable name="s1">
      <xsl:call-template name="replace">
        <xsl:with-param name="text"><xsl:value-of select="$text"/></xsl:with-param>
        <xsl:with-param name="search">\</xsl:with-param>
        <xsl:with-param name="replace">\\</xsl:with-param>
      </xsl:call-template>
    </xsl:variable>
    <!-- step 2: double quote -> \" -->
    <xsl:variable name="s2">
      <xsl:call-template name="replace">
        <xsl:with-param name="text"><xsl:value-of select="$s1"/></xsl:with-param>
        <xsl:with-param name="search">"</xsl:with-param>
        <xsl:with-param name="replace">\"</xsl:with-param>
      </xsl:call-template>
    </xsl:variable>
    <!-- step 3: newline -> \n -->
    <xsl:variable name="s3">
      <xsl:call-template name="replace">
        <xsl:with-param name="text"><xsl:value-of select="$s2"/></xsl:with-param>
        <xsl:with-param name="search">&#10;</xsl:with-param>
        <xsl:with-param name="replace">\n</xsl:with-param>
      </xsl:call-template>
    </xsl:variable>
    <!-- step 4: carriage return -> \r -->
    <xsl:variable name="s4">
      <xsl:call-template name="replace">
        <xsl:with-param name="text"><xsl:value-of select="$s3"/></xsl:with-param>
        <xsl:with-param name="search">&#13;</xsl:with-param>
        <xsl:with-param name="replace">\r</xsl:with-param>
      </xsl:call-template>
    </xsl:variable>
    <!-- step 5: tab -> \t -->
    <xsl:variable name="s5">
      <xsl:call-template name="replace">
        <xsl:with-param name="text"><xsl:value-of select="$s4"/></xsl:with-param>
        <xsl:with-param name="search">&#9;</xsl:with-param>
        <xsl:with-param name="replace">\t</xsl:with-param>
      </xsl:call-template>
    </xsl:variable>
    <xsl:value-of select="$s5"/>
  </xsl:template>

  <!-- Root template: build JSON object -->
  <xsl:template match="/xmldoc">
    <xsl:text>{</xsl:text>
    <!-- keyword -->
    <xsl:text>"keyword": "</xsl:text>
    <xsl:call-template name="escape-json"><xsl:with-param name="text" select="normalize-space(keyword)"/></xsl:call-template>
    <xsl:text>"</xsl:text>

    <!-- keyword_alias (array) -->
    <xsl:if test="keyword_alias">
      <xsl:text>, "keyword_alias": [</xsl:text>
      <xsl:for-each select="keyword_alias">
        <xsl:text>"</xsl:text>
        <xsl:call-template name="escape-json"><xsl:with-param name="text" select="normalize-space(.)"/></xsl:call-template>
        <xsl:text>"</xsl:text>
        <xsl:if test="position() != last()"><xsl:text>, </xsl:text></xsl:if>
      </xsl:for-each>
      <xsl:text>]</xsl:text>
    </xsl:if>

    <!-- Short description -->
    <xsl:text>, "short_description": "</xsl:text>
    <xsl:call-template name="escape-json"><xsl:with-param name="text" select="normalize-space(short_description)"/></xsl:call-template>
    <xsl:text>"</xsl:text>

    <!-- Syntax array -->
    <xsl:text>, "Syntax": [</xsl:text>
    <xsl:for-each select="syntax/syntax_item">
      <xsl:text>"</xsl:text>
      <xsl:call-template name="escape-json"><xsl:with-param name="text" select="normalize-space(.)"/></xsl:call-template>
      <xsl:text>"</xsl:text>
      <xsl:if test="position() != last()"><xsl:text>, </xsl:text></xsl:if>
    </xsl:for-each>
    <xsl:text>]</xsl:text>

    <!-- Input Arguments -->
    <xsl:text>, "Input Arguments": [</xsl:text>
    <xsl:for-each select="param_input/param_input_item">
      <xsl:text>{"name":"</xsl:text>
      <xsl:call-template name="escape-json"><xsl:with-param name="text" select="normalize-space(param_name)"/></xsl:call-template>
      <xsl:text>", "description":"</xsl:text>
      <xsl:apply-templates select="param_description"/>
      <xsl:text>"}</xsl:text>
      <xsl:if test="position() != last()"><xsl:text>, </xsl:text></xsl:if>
    </xsl:for-each>
    <xsl:text>]</xsl:text>

    <!-- Output Arguments -->
    <xsl:text>, "Output Arguments": [</xsl:text>
    <xsl:for-each select="param_output/param_output_item">
      <xsl:text>{"name":"</xsl:text>
      <xsl:call-template name="escape-json"><xsl:with-param name="text" select="normalize-space(param_name)"/></xsl:call-template>
      <xsl:text>", "description":"</xsl:text>
      <xsl:apply-templates select="param_description"/>
      <xsl:text>"}</xsl:text>
      <xsl:if test="position() != last()"><xsl:text>, </xsl:text></xsl:if>
    </xsl:for-each>
    <xsl:text>]</xsl:text>

    <!-- description (concatenate paragraphs) -->
    <xsl:text>, "description": "</xsl:text>
    <xsl:apply-templates select="description"/>
    <xsl:text>"</xsl:text>

    <!-- see also (array of link texts) -->
    <xsl:text>, "see also": [</xsl:text>
    <xsl:for-each select="see_also/see_also_item/link">
      <xsl:text>"</xsl:text>
      <xsl:call-template name="escape-json"><xsl:with-param name="text" select="normalize-space(.)"/></xsl:call-template>
      <xsl:text>"</xsl:text>
      <xsl:if test="position() != last()"><xsl:text>, </xsl:text></xsl:if>
    </xsl:for-each>
    <xsl:text>]</xsl:text>

    <!-- used function (optional field) -->
    <xsl:if test="used_function and normalize-space(used_function)">
      <xsl:text>, "used_function": "</xsl:text>
      <xsl:call-template name="escape-json"><xsl:with-param name="text" select="normalize-space(used_function)"/></xsl:call-template>
      <xsl:text>"</xsl:text>
    </xsl:if>

    <!-- copyright (optional field) -->
    <xsl:if test="copyright and normalize-space(copyright)">
      <xsl:text>, "copyright": "</xsl:text>
      <xsl:call-template name="escape-json"><xsl:with-param name="text" select="normalize-space(copyright)"/></xsl:call-template>
      <xsl:text>"</xsl:text>
    </xsl:if>

    <xsl:text>}</xsl:text>
  </xsl:template>

  <!-- Keep description text normalized and escaped -->
  <xsl:template match="description">
    <!-- Iterate only over non-empty child nodes and join them intelligently -->
    <xsl:for-each select="node()[normalize-space(.) != '']">
      <xsl:variable name="this">
        <xsl:apply-templates select="."/>
      </xsl:variable>
      <xsl:call-template name="escape-json">
        <xsl:with-param name="text" select="normalize-space($this)"/>
      </xsl:call-template>
      <!-- Add a single space between non-empty nodes unless the next non-empty node starts with punctuation -->
      <xsl:if test="position() != last()">
        <xsl:variable name="next" select="following-sibling::node()[normalize-space(.) != ''][1]"/>
        <xsl:variable name="nexttext" select="normalize-space($next)"/>
        <xsl:variable name="firstchar" select="substring($nexttext, 1, 1)"/>
        <xsl:if test="not(contains('.,;:!?', $firstchar))">
          <xsl:text> </xsl:text>
        </xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <!-- param_description: output normalized text and escape -->
  <xsl:template match="param_description">
    <!-- Build description from the string-value of the element (preserves spaces between nodes) -->
    <xsl:call-template name="escape-json">
      <xsl:with-param name="text" select="normalize-space(string(.))"/>
    </xsl:call-template>
  </xsl:template>

  <!-- Default text nodes: normalize-space -->
  <xsl:template match="text()">
    <!-- Output text node raw; higher-level templates will normalize as needed -->
    <xsl:value-of select="."/>
  </xsl:template>

</xsl:stylesheet>
