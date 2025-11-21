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
<xsl:stylesheet version="1.0"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:ext="http://io.github.nelson_lang/ext">

  <xsl:output method="text" encoding="UTF-8"/>

  <!-- Root template -->
  <xsl:template match="/xmldoc">

    <xsl:variable name="syntax-text">
      <xsl:choose>
        <xsl:when test="language = 'fr_FR'">📝 Syntaxe</xsl:when>
        <xsl:otherwise>📝 Syntax</xsl:otherwise>
      </xsl:choose>
    </xsl:variable>

    <xsl:variable name="input-argument-text">
      <xsl:choose>
        <xsl:when test="language = 'fr_FR'">📥 Argument d'entrée</xsl:when>
        <xsl:otherwise>📥 Input argument</xsl:otherwise>
      </xsl:choose>
    </xsl:variable>

    <xsl:variable name="output-argument-text">
      <xsl:choose>
        <xsl:when test="language = 'fr_FR'">📤 Argument de sortie</xsl:when>
        <xsl:otherwise>📤 Output argument</xsl:otherwise>
      </xsl:choose>
    </xsl:variable>

    <xsl:variable name="description-text">
      <xsl:choose>
        <xsl:when test="language = 'fr_FR'">📄 Description</xsl:when>
        <xsl:otherwise>📄 Description</xsl:otherwise>
      </xsl:choose>
    </xsl:variable>

    <xsl:variable name="bibliography-text">
      <xsl:choose>
        <xsl:when test="language = 'fr_FR'">📚 Bibliographie</xsl:when>
        <xsl:otherwise>📚 Bibliography</xsl:otherwise>
      </xsl:choose>
    </xsl:variable>

    <xsl:variable name="used-functions-text">
      <xsl:choose>
        <xsl:when test="language = 'fr_FR'">Fonction(s) utilisée(s)</xsl:when>
        <xsl:otherwise>Used function(s)</xsl:otherwise>
      </xsl:choose>
    </xsl:variable>

    <xsl:variable name="example-text">
      <xsl:choose>
        <xsl:when test="language = 'fr_FR'">💡 Exemple</xsl:when>
        <xsl:otherwise>💡 Example</xsl:otherwise>
      </xsl:choose>
    </xsl:variable>

    <xsl:variable name="see-also-text">
      <xsl:choose>
        <xsl:when test="language = 'fr_FR'">🔗 Voir aussi</xsl:when>
        <xsl:otherwise>🔗 See also</xsl:otherwise>
      </xsl:choose>
    </xsl:variable>

    <xsl:variable name="history-text">
      <xsl:choose>
        <xsl:when test="language = 'fr_FR'">🕔 Historique</xsl:when>
        <xsl:otherwise>🕔 History</xsl:otherwise>
      </xsl:choose>
    </xsl:variable>

    <xsl:variable name="author-text">
      <xsl:choose>
        <xsl:when test="language = 'fr_FR'">👤 Auteur</xsl:when>
        <xsl:otherwise>👤 Author</xsl:otherwise>
      </xsl:choose>
    </xsl:variable>

    <xsl:variable name="version-text">
      <xsl:choose>
        <xsl:when test="language = 'fr_FR'">Version</xsl:when>
        <xsl:otherwise>Version</xsl:otherwise>
      </xsl:choose>
    </xsl:variable>

    <xsl:text># </xsl:text>
    <xsl:call-template name="escape-description-text">
      <xsl:with-param name="text" select="keyword"/>
    </xsl:call-template>
    <xsl:text>&#10;&#10;</xsl:text>
    <xsl:call-template name="escape-description-text">
      <xsl:with-param name="text" select="short_description"/>
    </xsl:call-template>
    <xsl:text>&#10;</xsl:text>

    <!-- Chapter description section if present -->
    <xsl:if test="chapter_description">
    <xsl:text>&#10;</xsl:text>
    <xsl:apply-templates select="chapter_description"/>
    <xsl:text>&#10;</xsl:text>
    </xsl:if>

    <!-- Only display syntax section if it has content -->
    <xsl:if test="syntax/syntax_item[normalize-space(.)]">
    <xsl:text>&#10;## </xsl:text><xsl:value-of select="$syntax-text"/><xsl:text>&#10;&#10;</xsl:text>
    <xsl:for-each select="syntax/syntax_item">
    <xsl:text>- </xsl:text>
    <xsl:variable name="syntax-line" select="normalize-space(.)"/>
    <xsl:call-template name="escape-description-text">
      <xsl:with-param name="text" select="$syntax-line"/>
    </xsl:call-template>
    <xsl:text>&#10;</xsl:text>
    </xsl:for-each>
    </xsl:if>

    <!-- Only show input arguments section if it has items -->
    <xsl:if test="param_input/param_input_item">
    <xsl:text>&#10;## </xsl:text><xsl:value-of select="$input-argument-text"/><xsl:text>&#10;&#10;</xsl:text>
    <xsl:for-each select="param_input/param_input_item">
  <xsl:text>- </xsl:text>
  <xsl:call-template name="escape-markdown-text">
    <xsl:with-param name="text" select="normalize-space(param_name)"/>
  </xsl:call-template>
  <xsl:text> - </xsl:text>
  <xsl:apply-templates select="param_description"/>
    <xsl:text>&#10;</xsl:text>
    </xsl:for-each>
    </xsl:if>

    <!-- Only show output arguments section if it has items -->
    <xsl:if test="param_output/param_output_item">
    <xsl:text>&#10;## </xsl:text><xsl:value-of select="$output-argument-text"/><xsl:text>&#10;&#10;</xsl:text>
    <xsl:for-each select="param_output/param_output_item">
  <xsl:text>- </xsl:text>
  <xsl:call-template name="escape-markdown-text">
    <xsl:with-param name="text" select="normalize-space(param_name)"/>
  </xsl:call-template>
  <xsl:text> - </xsl:text>
  <xsl:apply-templates select="param_description"/>
    <xsl:text>&#10;</xsl:text>
    </xsl:for-each>
    </xsl:if>

    <!-- Only show description section if it's not empty -->
    <xsl:if test="description">
    <xsl:text>&#10;## </xsl:text><xsl:value-of select="$description-text"/><xsl:text>&#10;</xsl:text>
    <xsl:apply-templates select="description"/>
    <xsl:text>&#10;</xsl:text>
    </xsl:if>

    <!-- Used function(s) section if present and non-empty -->
    <xsl:if test="used_function and normalize-space(used_function) != ''">
      <xsl:text>&#10;## </xsl:text><xsl:value-of select="$used-functions-text"/><xsl:text>&#10;&#10;</xsl:text>
      <xsl:value-of select="used_function"/>
      <xsl:text>&#10;</xsl:text>
    </xsl:if>

    <!-- Bibliography section if present and non-empty -->
    <xsl:if test="bibliography and normalize-space(bibliography) != ''">
      <xsl:text>&#10;## </xsl:text><xsl:value-of select="$bibliography-text"/><xsl:text>&#10;&#10;</xsl:text>
      <xsl:value-of select="bibliography"/>
      <xsl:text>&#10;</xsl:text>
    </xsl:if>

    <!-- Only show examples section if there are examples -->
    <xsl:if test="examples/example_item">
    <xsl:text>&#10;## </xsl:text><xsl:value-of select="$example-text"/>
    <xsl:if test="count(examples/example_item) > 1">
      <xsl:text>s</xsl:text>
    </xsl:if>
    <xsl:text>&#10;&#10;</xsl:text>
    <xsl:for-each select="examples/example_item">
      <xsl:apply-templates select="example_item_description"/>
      <xsl:text>&#10;</xsl:text>
      <xsl:if test="example_item_data">
        <xsl:text>```matlab&#10;</xsl:text>
        <xsl:call-template name="process-example-data">
          <xsl:with-param name="text" select="example_item_data"/>
        </xsl:call-template>
        <xsl:text>&#10;```&#10;</xsl:text>
      </xsl:if>
      <xsl:if test="example_item_img">
        <xsl:variable name="imgsrc" select="ext:copy_img(example_item_img/@src)"/>
        <xsl:text>&lt;img src="</xsl:text>
        <xsl:value-of select="substring-after($imgsrc, './')"/>
        <xsl:text>"</xsl:text>
        <xsl:if test="example_item_img/@align">
          <xsl:text> align="</xsl:text>
          <xsl:value-of select="example_item_img/@align"/>
          <xsl:text>"</xsl:text>
        </xsl:if>
        <xsl:text>/&gt;&#10;</xsl:text>
      </xsl:if>
    </xsl:for-each>
    </xsl:if>

    <!-- Only show "See also" section if it has items -->
    <xsl:if test="see_also/see_also_item/link">
    <xsl:text>&#10;&#10;## </xsl:text><xsl:value-of select="$see-also-text"/><xsl:text>&#10;&#10;</xsl:text>
    <xsl:for-each select="see_also/see_also_item/link">
      <xsl:text>[</xsl:text><xsl:value-of select="."/><xsl:text>](</xsl:text>
      <!-- Extract module and function name from linkend -->
      <xsl:variable name="linkend" select="@linkend"/>
      <xsl:variable name="module">
        <xsl:choose>
          <xsl:when test="contains($linkend, '${') and contains($linkend, '}')">
            <xsl:value-of select="substring-before(substring-after($linkend, '${'), '}')"/>
          </xsl:when>
          <xsl:when test="contains($linkend, '{') and contains($linkend, '}')">
            <xsl:value-of select="substring-before(substring-after($linkend, '{'), '}')"/>
          </xsl:when>
        </xsl:choose>
      </xsl:variable>
      <xsl:variable name="function">
        <xsl:choose>
          <xsl:when test="contains($linkend, '}')">
            <xsl:value-of select="substring-after($linkend, '}')"/>
          </xsl:when>
          <xsl:otherwise>
            <xsl:value-of select="$linkend"/>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:variable>
      
      <!-- Create the proper relative path with module reference -->
      <xsl:choose>
        <xsl:when test="string-length($module) > 0">
          <xsl:text>../</xsl:text><xsl:value-of select="$module"/><xsl:text>/</xsl:text><xsl:value-of select="$function"/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:value-of select="$function"/>
        </xsl:otherwise>
      </xsl:choose>
      <xsl:text>.md)</xsl:text>
      <xsl:if test="position() != last()"><xsl:text>, </xsl:text></xsl:if>
    </xsl:for-each>
    <xsl:text>.</xsl:text>
    </xsl:if>

    <!-- Only show history section if it has items -->
    <xsl:if test="history/history_item">
    <xsl:text>&#10;&#10;## </xsl:text><xsl:value-of select="$history-text"/><xsl:text>&#10;&#10;</xsl:text>
    <xsl:text>| </xsl:text><xsl:value-of select="$version-text"/><xsl:text> | </xsl:text><xsl:value-of select="$description-text"/><xsl:text>     |&#10;</xsl:text>
    <xsl:text>| ------- | --------------- |&#10;</xsl:text>
    <xsl:for-each select="history/history_item">
    <xsl:text>| </xsl:text><xsl:value-of select="history_version"/><xsl:text>   | </xsl:text><xsl:value-of select="history_description"/><xsl:text> |&#10;</xsl:text>
    </xsl:for-each>
    </xsl:if>

    <!-- Only show author section if there are authors
         -> Emit authors inside an HTML comment so they appear in the generated file
            but are not rendered in the Markdown output.
         -> Keep localization by using $author-text.
    -->
    <xsl:if test="authors/author_item">
      <xsl:text>&#10;&lt;!--&#10;## </xsl:text><xsl:value-of select="$author-text"/><xsl:text>&#10;&#10;</xsl:text>
      <xsl:for-each select="authors/author_item">
        <xsl:value-of select="."/>
        <xsl:if test="position() != last()">
          <xsl:text>&#10;</xsl:text>
        </xsl:if>
      </xsl:for-each>
      <xsl:text>&#10;--&gt;&#10;</xsl:text>
    </xsl:if>
  </xsl:template>

  <!-- Helper template to properly process example data including CDATA -->
  <xsl:template name="process-example-data">
    <xsl:param name="text"/>
    <xsl:variable name="raw">
      <xsl:choose>
        <xsl:when test="contains($text, '[CDATA[')">
          <xsl:value-of select="substring-before(substring-after($text, '[CDATA['), ']]')"/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:value-of select="$text"/>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <xsl:value-of select="$raw"/>
  </xsl:template>

  <!-- By default, output the content with proper whitespace handling -->
  <xsl:template match="text()">
    <xsl:choose>
      <xsl:when test="normalize-space(.) = ''">
        <!-- If text is only whitespace, output single space if between inline elements -->
        <xsl:if test="preceding-sibling::node() and following-sibling::node()">
          <xsl:text> </xsl:text>
        </xsl:if>
      </xsl:when>
      <xsl:otherwise>
        <!-- Output normalized text content with space preservation -->
        <xsl:if test="preceding-sibling::node() and starts-with(., ' ')">
          <xsl:text> </xsl:text>
        </xsl:if>
        <xsl:variable name="normalized" select="normalize-space(.)"/>
        <xsl:variable name="escaped">
          <xsl:call-template name="escape-description-text">
            <xsl:with-param name="text" select="$normalized"/>
          </xsl:call-template>
        </xsl:variable>
        <xsl:value-of select="$escaped"/>
        <xsl:if test="following-sibling::node() and substring(., string-length(.)) = ' '">
          <xsl:text> </xsl:text>
        </xsl:if>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <!-- Ensure underscores are escaped inside parameter descriptions -->
  <xsl:template match="param_description//text()" priority="2">
    <xsl:choose>
      <xsl:when test="normalize-space(.) = ''">
        <xsl:if test="preceding-sibling::node() and following-sibling::node()">
          <xsl:text> </xsl:text>
        </xsl:if>
      </xsl:when>
      <xsl:otherwise>
        <xsl:if test="preceding-sibling::node() and starts-with(., ' ')">
          <xsl:text> </xsl:text>
        </xsl:if>
        <xsl:variable name="normalized" select="normalize-space(.)"/>
        <xsl:variable name="escaped">
          <xsl:call-template name="escape-markdown-text">
            <xsl:with-param name="text" select="$normalized"/>
          </xsl:call-template>
        </xsl:variable>
        <xsl:value-of select="$escaped"/>
        <xsl:if test="following-sibling::node() and substring(., string-length(.)) = ' '">
          <xsl:text> </xsl:text>
        </xsl:if>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <!-- Render inline <link> elements in markdown -->
  <xsl:template match="link">
    <xsl:variable name="linkend" select="@linkend"/>
    <xsl:variable name="module">
      <xsl:choose>
        <xsl:when test="contains($linkend, '${') and contains($linkend, '}')">
          <xsl:value-of select="substring-before(substring-after($linkend, '${'), '}')"/>
        </xsl:when>
        <xsl:when test="contains($linkend, '{') and contains($linkend, '}')">
          <xsl:value-of select="substring-before(substring-after($linkend, '{'), '}')"/>
        </xsl:when>
      </xsl:choose>
    </xsl:variable>
    <xsl:variable name="function">
      <xsl:choose>
        <xsl:when test="contains($linkend, '}')">
          <xsl:value-of select="substring-after($linkend, '}')"/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:value-of select="$linkend"/>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <xsl:text>[</xsl:text><xsl:value-of select="."/><xsl:text>](</xsl:text>
    <xsl:choose>
      <xsl:when test="string-length($module) &gt; 0">
        <xsl:text>../</xsl:text><xsl:value-of select="$module"/><xsl:text>/</xsl:text><xsl:value-of select="$function"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="$function"/>
      </xsl:otherwise>
    </xsl:choose>
    <xsl:text>.md)</xsl:text>
  </xsl:template>

  <xsl:template match="example_item">
    <xsl:apply-templates select="example_item_description"/>
  </xsl:template>

  <xsl:template match="example_item_description">
    <xsl:value-of select="."/>
    <xsl:text>&#10;</xsl:text>
  </xsl:template>

  <!-- Template for <description> -->
  <xsl:template match="description">
    <xsl:apply-templates/>
  </xsl:template>

  <!-- Template for <p> tag -->
  <xsl:template match="p">
    <xsl:text>&#10;&#10;</xsl:text>
    <xsl:apply-templates/>
  </xsl:template>

  <!-- Template for <img> tag -->
  <xsl:template match="img">
    <xsl:variable name="imgsrc" select="ext:copy_img(@src)"/>
    <xsl:text>&#10;</xsl:text>
    <xsl:text>&lt;img src="</xsl:text>
    <xsl:value-of select="substring-after($imgsrc, './')"/>
    <xsl:text>"</xsl:text>
    <xsl:if test="@align">
      <xsl:text> align="</xsl:text><xsl:value-of select="@align"/><xsl:text>"</xsl:text>
    </xsl:if>
    <xsl:if test="@alt">
      <xsl:text> alt="</xsl:text><xsl:value-of select="@alt"/><xsl:text>"</xsl:text>
    </xsl:if>
    <xsl:if test="@width">
      <xsl:text> width="</xsl:text><xsl:value-of select="@width"/><xsl:text>"</xsl:text>
    </xsl:if>
    <xsl:if test="@height">
      <xsl:text> height="</xsl:text><xsl:value-of select="@height"/><xsl:text>"</xsl:text>
    </xsl:if>
    <xsl:text>/&gt;&#10;</xsl:text>
  </xsl:template>

  <!-- Template for <latex> tag -->
  <xsl:template match="latex">
    <xsl:text>&#10;$$</xsl:text>
    <xsl:value-of select="normalize-space(.)"/>
    <xsl:text>$$&#10;</xsl:text>
  </xsl:template>

  <!-- Template for <ul> (unordered list) -->
  <xsl:template match="ul">
    <xsl:text>&#10;</xsl:text>
    <xsl:apply-templates/>
  </xsl:template>

  <!-- Template for <li> (list item) in unordered lists -->
  <xsl:template match="ul/li">
    <xsl:text>&#10;- </xsl:text>
    <xsl:apply-templates/>
  </xsl:template>

  <!-- Template for <ol> (ordered list) -->
  <xsl:template match="ol">
    <xsl:apply-templates/>
    <xsl:text>&#10;</xsl:text>
  </xsl:template>

  <!-- Template for <li> in ordered lists -->
  <xsl:template match="ol/li">
    <xsl:text>&#10;</xsl:text>
    <xsl:value-of select="position()"/>
    <xsl:text>. </xsl:text>
    <xsl:apply-templates/>
  </xsl:template>

  <!-- Table rendering templates -->
  <xsl:template match="table">
    <xsl:if test="not(preceding-sibling::*[1][self::p and not(node())])">
      <xsl:text>&#10;</xsl:text>
    </xsl:if>
    <xsl:text>&#10;</xsl:text>
    <xsl:apply-templates select="tr[1]" mode="header"/>
    <xsl:apply-templates select="tr[position() > 1]"/>
    <xsl:text>&#10;</xsl:text>
  </xsl:template>

  <xsl:template match="tr" mode="header">
    <!-- Render header row: compact single-line cells -->
    <xsl:variable name="cells" select="th | td"/>
    <xsl:if test="count($cells)">
    <xsl:text>| </xsl:text>
    <xsl:for-each select="$cells">
      <xsl:apply-templates/>
      <xsl:text> | </xsl:text>
    </xsl:for-each>
    <xsl:text>&#10;| </xsl:text>
    <xsl:for-each select="$cells">
      <xsl:text>--- | </xsl:text>
    </xsl:for-each>
    <xsl:text>&#10;</xsl:text>
    </xsl:if>
  </xsl:template>

  <xsl:template match="tr">
    <!-- Render data row with normalized, single-line cell content -->
    <xsl:variable name="cells" select="td | th"/>
    <xsl:if test="count($cells)">
      <xsl:text>| </xsl:text>
      <xsl:for-each select="$cells">
        <xsl:apply-templates/>
        <xsl:text> | </xsl:text>
      </xsl:for-each>
      <xsl:text>&#10;</xsl:text>
    </xsl:if>
  </xsl:template>

  <xsl:template match="th">
    <!-- Output header cell text -->
    <xsl:apply-templates/>
  </xsl:template>

  <xsl:template match="td">
    <!-- Output data cell text -->
    <xsl:apply-templates/>
  </xsl:template>

  <!-- Render inline bold/italic content as Markdown inside tables -->
  <xsl:template match="table//b">
    <xsl:text>**</xsl:text>
    <xsl:call-template name="escape-markdown-text">
      <xsl:with-param name="text" select="normalize-space(.)"/>
    </xsl:call-template>
    <xsl:text>**</xsl:text>
  </xsl:template>
  
  <xsl:template match="table//i">
    <xsl:text>*</xsl:text>
    <xsl:call-template name="escape-markdown-text">
      <xsl:with-param name="text" select="normalize-space(.)"/>
    </xsl:call-template>
    <xsl:text>*</xsl:text>
  </xsl:template>

  <!-- Table-specific text nodes require markdown escaping (pipes, backslashes) -->
  <xsl:template match="table//text()" priority="1">
    <xsl:choose>
      <xsl:when test="normalize-space(.) = ''">
        <xsl:if test="preceding-sibling::node() and following-sibling::node()">
          <xsl:text> </xsl:text>
        </xsl:if>
      </xsl:when>
      <xsl:otherwise>
        <xsl:if test="preceding-sibling::node() and starts-with(., ' ')">
          <xsl:text> </xsl:text>
        </xsl:if>
        <xsl:call-template name="escape-markdown-text">
          <xsl:with-param name="text" select="normalize-space(.)"/>
        </xsl:call-template>
        <xsl:if test="following-sibling::node() and substring(., string-length(.)) = ' '">
          <xsl:text> </xsl:text>
        </xsl:if>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <!-- General templates for inline formatting (outside tables) -->
  <xsl:template match="b">
    <xsl:text>&lt;b&gt;</xsl:text>
    <xsl:call-template name="escape-markdown-text">
      <xsl:with-param name="text" select="."/>
    </xsl:call-template>
    <xsl:text>&lt;/b&gt;</xsl:text>
  </xsl:template>

  <xsl:template match="i">
    <xsl:text>&lt;i&gt;</xsl:text>
    <xsl:call-template name="escape-markdown-text">
      <xsl:with-param name="text" select="."/>
    </xsl:call-template>
    <xsl:text>&lt;/i&gt;</xsl:text>
  </xsl:template>

  <xsl:template match="code">
    <xsl:text>&lt;code&gt;</xsl:text>
    <xsl:value-of select="."/>
    <xsl:text>&lt;/code&gt;</xsl:text>
  </xsl:template>

  <!-- Template for chapter_description -->
  <xsl:template match="chapter_description">
    <xsl:apply-templates/>
  </xsl:template>

  <!-- Markdown escaping helpers -->
  <xsl:template name="escape-markdown-text">
    <xsl:param name="text"/>
    <xsl:variable name="escaped-backslash">
      <xsl:call-template name="replace-string">
        <xsl:with-param name="text" select="$text"/>
        <xsl:with-param name="search" select="'\'"/>
        <xsl:with-param name="replace" select="'\\'"/>
      </xsl:call-template>
    </xsl:variable>
    <xsl:variable name="escaped-pipe">
      <xsl:call-template name="replace-string">
        <xsl:with-param name="text" select="$escaped-backslash"/>
        <xsl:with-param name="search" select="'|'"/>
        <xsl:with-param name="replace" select="'\|'"/>
      </xsl:call-template>
    </xsl:variable>
    <xsl:variable name="escaped-underscore">
      <xsl:call-template name="replace-string">
        <xsl:with-param name="text" select="$escaped-pipe"/>
        <xsl:with-param name="search" select="'_'"/>
        <xsl:with-param name="replace" select="'\_'"/>
      </xsl:call-template>
    </xsl:variable>
    <xsl:variable name="escaped-asterisk">
      <xsl:call-template name="replace-string">
        <xsl:with-param name="text" select="$escaped-underscore"/>
        <xsl:with-param name="search" select="'*'"/>
        <xsl:with-param name="replace" select="'\*'"/>
      </xsl:call-template>
    </xsl:variable>
    <xsl:value-of select="$escaped-asterisk"/>
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

  <!-- Narrative text uses the general markdown escaping rules -->
  <xsl:template name="escape-description-text">
    <xsl:param name="text"/>
    <xsl:call-template name="escape-markdown-text">
      <xsl:with-param name="text" select="$text"/>
    </xsl:call-template>
  </xsl:template>

</xsl:stylesheet>