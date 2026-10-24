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
  <xsl:import href="nelson_typst_common.xslt"/>

  <xsl:output method="text" encoding="UTF-8"/>

  <!-- Attributes injected by XmlTransform on the root element -->
  <xsl:variable name="module" select="/*/@module"/>
  <xsl:variable name="rootpath" select="/*/@rootpath"/>
  <xsl:variable name="page-id">
    <xsl:choose>
      <xsl:when test="string-length(/*/@canonical_link) &gt; 4">
        <xsl:value-of select="substring(/*/@canonical_link, 1, string-length(/*/@canonical_link) - 4)"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="normalize-space(/xmldoc/keyword)"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:variable>
  <xsl:variable name="is-french" select="/xmldoc/language = 'fr_FR'"/>

  <!-- Root template -->
  <xsl:template match="/xmldoc">
    <xsl:text>#import "</xsl:text>
    <xsl:value-of select="$rootpath"/>
    <xsl:text>nelson_help.typ": *&#10;&#10;</xsl:text>

    <xsl:text>= </xsl:text>
    <xsl:call-template name="escape-typst-text">
      <xsl:with-param name="text" select="normalize-space(keyword)"/>
    </xsl:call-template>
    <xsl:text> </xsl:text>
    <xsl:call-template name="page-label">
      <xsl:with-param name="module" select="$module"/>
      <xsl:with-param name="page" select="$page-id"/>
    </xsl:call-template>
    <xsl:text>&#10;&#10;</xsl:text>

    <xsl:for-each select="description/p[img[contains(@src, 'libraries/') and contains(@src, 'exports/')]]">
      <xsl:apply-templates select="img" mode="centered"/>
    </xsl:for-each>

    <xsl:call-template name="escape-typst-text">
      <xsl:with-param name="text" select="normalize-space(short_description)"/>
    </xsl:call-template>
    <xsl:text>&#10;</xsl:text>

    <xsl:if test="chapter_description">
      <xsl:text>&#10;</xsl:text>
      <xsl:apply-templates select="chapter_description"/>
    </xsl:if>

    <xsl:if test="syntax/syntax_item[normalize-space(.)]">
      <xsl:call-template name="section-title">
        <xsl:with-param name="en">Syntax</xsl:with-param>
        <xsl:with-param name="fr">Syntaxe</xsl:with-param>
      </xsl:call-template>
      <xsl:for-each select="syntax/syntax_item[normalize-space(.)]">
        <xsl:text>- </xsl:text>
        <xsl:call-template name="raw-inline">
          <xsl:with-param name="text" select="normalize-space(.)"/>
        </xsl:call-template>
        <xsl:text>&#10;</xsl:text>
      </xsl:for-each>
    </xsl:if>

    <xsl:if test="param_input/param_input_item">
      <xsl:call-template name="section-title">
        <xsl:with-param name="en">Input argument</xsl:with-param>
        <xsl:with-param name="fr">Argument d'entrée</xsl:with-param>
      </xsl:call-template>
      <xsl:apply-templates select="param_input/param_input_item"/>
    </xsl:if>

    <xsl:if test="param_output/param_output_item">
      <xsl:call-template name="section-title">
        <xsl:with-param name="en">Output argument</xsl:with-param>
        <xsl:with-param name="fr">Argument de sortie</xsl:with-param>
      </xsl:call-template>
      <xsl:apply-templates select="param_output/param_output_item"/>
    </xsl:if>

    <xsl:if test="description[normalize-space(.) != '' or .//img]">
      <xsl:call-template name="section-title">
        <xsl:with-param name="en">Description</xsl:with-param>
        <xsl:with-param name="fr">Description</xsl:with-param>
      </xsl:call-template>
      <xsl:apply-templates select="description/node()[not(self::p[img[contains(@src, 'libraries/') and contains(@src, 'exports/')]])]"/>
    </xsl:if>

    <xsl:if test="used_function and normalize-space(used_function) != ''">
      <xsl:call-template name="section-title">
        <xsl:with-param name="en">Used function(s)</xsl:with-param>
        <xsl:with-param name="fr">Fonction(s) utilisée(s)</xsl:with-param>
      </xsl:call-template>
      <xsl:call-template name="escape-typst-text">
        <xsl:with-param name="text" select="normalize-space(used_function)"/>
      </xsl:call-template>
      <xsl:text>&#10;</xsl:text>
    </xsl:if>

    <xsl:if test="bibliography and normalize-space(bibliography) != ''">
      <xsl:call-template name="section-title">
        <xsl:with-param name="en">Bibliography</xsl:with-param>
        <xsl:with-param name="fr">Bibliographie</xsl:with-param>
      </xsl:call-template>
      <xsl:call-template name="escape-typst-text">
        <xsl:with-param name="text" select="normalize-space(bibliography)"/>
      </xsl:call-template>
      <xsl:text>&#10;</xsl:text>
    </xsl:if>

    <xsl:if test="examples/example_item">
      <xsl:call-template name="section-title">
        <xsl:with-param name="en">
          <xsl:text>Example</xsl:text>
          <xsl:if test="count(examples/example_item) > 1">s</xsl:if>
        </xsl:with-param>
        <xsl:with-param name="fr">
          <xsl:text>Exemple</xsl:text>
          <xsl:if test="count(examples/example_item) > 1">s</xsl:if>
        </xsl:with-param>
      </xsl:call-template>
      <xsl:apply-templates select="examples/example_item"/>
    </xsl:if>

    <xsl:if test="see_also/see_also_item/link">
      <xsl:call-template name="section-title">
        <xsl:with-param name="en">See also</xsl:with-param>
        <xsl:with-param name="fr">Voir aussi</xsl:with-param>
      </xsl:call-template>
      <xsl:for-each select="see_also/see_also_item/link">
        <xsl:apply-templates select="."/>
        <xsl:if test="position() != last()">
          <xsl:text>, </xsl:text>
        </xsl:if>
      </xsl:for-each>
      <xsl:text>.&#10;</xsl:text>
    </xsl:if>

    <xsl:if test="history/history_item">
      <xsl:call-template name="section-title">
        <xsl:with-param name="en">History</xsl:with-param>
        <xsl:with-param name="fr">Historique</xsl:with-param>
      </xsl:call-template>
      <xsl:text>#table(&#10;  columns: 2,&#10;  table.header([Version], [</xsl:text>
      <xsl:choose>
        <xsl:when test="$is-french">Description</xsl:when>
        <xsl:otherwise>Description</xsl:otherwise>
      </xsl:choose>
      <xsl:text>]),&#10;</xsl:text>
      <xsl:for-each select="history/history_item">
        <xsl:text>  [</xsl:text>
        <xsl:call-template name="escape-typst-text">
          <xsl:with-param name="text" select="normalize-space(history_version)"/>
        </xsl:call-template>
        <xsl:text>], [</xsl:text>
        <xsl:call-template name="escape-typst-text">
          <xsl:with-param name="text" select="normalize-space(history_description)"/>
        </xsl:call-template>
        <xsl:text>],&#10;</xsl:text>
      </xsl:for-each>
      <xsl:text>)&#10;</xsl:text>
    </xsl:if>

    <!-- Authors are kept as comments: present in the source, not rendered. -->
    <xsl:if test="authors/author_item">
      <xsl:text>&#10;</xsl:text>
      <xsl:for-each select="authors/author_item">
        <xsl:text>// </xsl:text>
        <xsl:choose>
          <xsl:when test="$is-french">Auteur</xsl:when>
          <xsl:otherwise>Author</xsl:otherwise>
        </xsl:choose>
        <xsl:text>: </xsl:text>
        <xsl:value-of select="normalize-space(.)"/>
        <xsl:text>&#10;</xsl:text>
      </xsl:for-each>
    </xsl:if>
  </xsl:template>

  <!-- Level-2 section title, localized -->
  <xsl:template name="section-title">
    <xsl:param name="en"/>
    <xsl:param name="fr"/>
    <xsl:text>&#10;== </xsl:text>
    <xsl:choose>
      <xsl:when test="$is-french"><xsl:value-of select="$fr"/></xsl:when>
      <xsl:otherwise><xsl:value-of select="$en"/></xsl:otherwise>
    </xsl:choose>
    <xsl:text>&#10;&#10;</xsl:text>
  </xsl:template>

  <!-- Input / output arguments as a term list -->
  <xsl:template match="param_input_item | param_output_item">
    <xsl:text>/ </xsl:text>
    <xsl:call-template name="escape-typst-text">
      <xsl:with-param name="text" select="normalize-space(param_name)"/>
    </xsl:call-template>
    <xsl:text>: </xsl:text>
    <xsl:apply-templates select="param_description"/>
    <xsl:text>&#10;</xsl:text>
  </xsl:template>

  <xsl:template match="example_item">
    <xsl:if test="normalize-space(example_item_description) != ''">
      <xsl:apply-templates select="example_item_description"/>
      <xsl:text>&#10;&#10;</xsl:text>
    </xsl:if>
    <xsl:if test="example_item_data">
      <xsl:text>``````matlab&#10;</xsl:text>
      <xsl:value-of select="example_item_data"/>
      <xsl:text>&#10;``````&#10;&#10;</xsl:text>
    </xsl:if>
    <xsl:if test="example_item_img">
      <xsl:variable name="imgsrc" select="ext:copy_img(example_item_img/@src, example_item_img/@generate)"/>
      <xsl:if test="string-length($imgsrc) &gt; 0">
        <xsl:call-template name="image">
          <xsl:with-param name="src" select="substring-after($imgsrc, './')"/>
          <xsl:with-param name="alt" select="example_item_img/@alt"/>
          <xsl:with-param name="centered" select="boolean(example_item_img/@align)"/>
        </xsl:call-template>
      </xsl:if>
    </xsl:if>
  </xsl:template>

  <!-- Text nodes: normalized whitespace, Typst markup escaped -->
  <xsl:template match="text()">
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

  <!-- Cross-reference to another page -->
  <xsl:template match="link">
    <xsl:call-template name="link-reference">
      <xsl:with-param name="module" select="$module"/>
    </xsl:call-template>
  </xsl:template>

  <!-- External hyperlink -->
  <xsl:template match="a">
    <xsl:text>#link(</xsl:text>
    <xsl:call-template name="typst-string">
      <xsl:with-param name="text" select="@href"/>
    </xsl:call-template>
    <xsl:text>)[</xsl:text>
    <xsl:apply-templates/>
    <xsl:text>];</xsl:text>
  </xsl:template>

  <xsl:template match="example_item_description | description | chapter_description | param_description">
    <xsl:apply-templates/>
  </xsl:template>

  <xsl:template match="p">
    <xsl:apply-templates/>
    <xsl:text>&#10;&#10;</xsl:text>
  </xsl:template>

  <xsl:template match="br">
    <xsl:text> \&#10;</xsl:text>
  </xsl:template>

  <!-- Block library icons (SVG exports of libraries/<lib>/exports/) are small
       assets: like the HTML emitter, they are shown with block-icon (capped
       at 240 x 180 px, bordered, centered). -->
  <xsl:template match="img">
    <xsl:variable name="imgsrc" select="ext:copy_img(@src)"/>
    <xsl:call-template name="image">
      <xsl:with-param name="src" select="substring-after($imgsrc, './')"/>
      <xsl:with-param name="alt" select="@alt"/>
      <xsl:with-param name="centered" select="boolean(@align)"/>
      <xsl:with-param name="icon" select="substring($imgsrc, string-length($imgsrc) - 3) = '.svg' and contains(@src, 'libraries/') and contains(@src, 'exports/')"/>
    </xsl:call-template>
  </xsl:template>

  <xsl:template match="img" mode="centered">
    <xsl:variable name="imgsrc" select="ext:copy_img(@src)"/>
    <xsl:call-template name="image">
      <xsl:with-param name="src" select="substring-after($imgsrc, './')"/>
      <xsl:with-param name="alt" select="@alt"/>
      <xsl:with-param name="centered" select="true()"/>
      <xsl:with-param name="icon" select="substring($imgsrc, string-length($imgsrc) - 3) = '.svg' and contains(@src, 'libraries/') and contains(@src, 'exports/')"/>
    </xsl:call-template>
  </xsl:template>

  <xsl:template name="image">
    <xsl:param name="src"/>
    <xsl:param name="alt"/>
    <xsl:param name="centered"/>
    <xsl:param name="icon" select="false()"/>
    <xsl:text>&#10;</xsl:text>
    <xsl:choose>
      <xsl:when test="$icon">
        <xsl:text>#block-icon(image(</xsl:text>
      </xsl:when>
      <xsl:otherwise>
        <xsl:if test="$centered">
          <xsl:text>#align(center)[</xsl:text>
        </xsl:if>
        <xsl:text>#image(</xsl:text>
      </xsl:otherwise>
    </xsl:choose>
    <xsl:call-template name="typst-string">
      <xsl:with-param name="text" select="$src"/>
    </xsl:call-template>
    <xsl:if test="string-length(normalize-space($alt)) &gt; 0">
      <xsl:text>, alt: </xsl:text>
      <xsl:call-template name="typst-string">
        <xsl:with-param name="text" select="normalize-space($alt)"/>
      </xsl:call-template>
    </xsl:if>
    <xsl:text>)</xsl:text>
    <xsl:if test="$icon">
      <!-- own paragraph, so that the following text does not join it -->
      <xsl:text>)&#10;</xsl:text>
    </xsl:if>
    <xsl:if test="$centered and not($icon)">
      <xsl:text>]</xsl:text>
    </xsl:if>
    <xsl:text>&#10;</xsl:text>
  </xsl:template>

  <!-- Source listing injected at build time -->
  <!-- Whole-file listings (no start/end bounds) are only referenced by path:
       printed in full they would add thousands of pages of duplicated
       manifests and handlers (the HTML help keeps them in a folded section).
       Bounded excerpts are printed. -->
  <xsl:template match="source_code[not(@start) and not(@end)]">
    <xsl:text>&#10;&#10;#source-ref(</xsl:text>
    <xsl:call-template name="typst-string">
      <xsl:with-param name="text" select="@path"/>
    </xsl:call-template>
    <xsl:if test="@label">
      <xsl:text>, title: </xsl:text>
      <xsl:call-template name="typst-string">
        <xsl:with-param name="text" select="@label"/>
      </xsl:call-template>
    </xsl:if>
    <xsl:text>)&#10;&#10;</xsl:text>
  </xsl:template>

  <xsl:template match="source_code">
    <xsl:text>&#10;&#10;#source-code(</xsl:text>
    <xsl:call-template name="typst-string">
      <xsl:with-param name="text" select="@path"/>
    </xsl:call-template>
    <xsl:if test="@label">
      <xsl:text>, title: </xsl:text>
      <xsl:call-template name="typst-string">
        <xsl:with-param name="text" select="@label"/>
      </xsl:call-template>
    </xsl:if>
    <xsl:text>)[&#10;``````</xsl:text>
    <xsl:choose>
      <xsl:when test="@language"><xsl:value-of select="@language"/></xsl:when>
      <xsl:when test="contains(@path, '.json')">json</xsl:when>
      <xsl:when test="contains(@path, '.cpp') or contains(@path, '.hpp') or contains(@path, '.h')">cpp</xsl:when>
      <xsl:otherwise>text</xsl:otherwise>
    </xsl:choose>
    <xsl:text>&#10;</xsl:text>
    <xsl:value-of select="ext:source_code(@path, string(@start), string(@end))"/>
    <xsl:text>&#10;``````&#10;]&#10;</xsl:text>
  </xsl:template>

  <xsl:template match="latex">
    <xsl:text>#latex(</xsl:text>
    <xsl:call-template name="typst-string">
      <xsl:with-param name="text" select="normalize-space(.)"/>
    </xsl:call-template>
    <xsl:text>);</xsl:text>
  </xsl:template>

  <xsl:template match="ul | ol">
    <xsl:text>&#10;</xsl:text>
    <xsl:apply-templates select="li"/>
  </xsl:template>

  <xsl:template match="ul/li">
    <xsl:text>&#10;- </xsl:text>
    <xsl:apply-templates/>
  </xsl:template>

  <xsl:template match="ol/li">
    <xsl:text>&#10;+ </xsl:text>
    <xsl:apply-templates/>
  </xsl:template>

  <!-- Tables: header row from thead when present, one [cell] per td/th -->
  <xsl:template match="table">
    <xsl:variable name="header" select="thead/tr[1]"/>
    <xsl:variable name="rows" select="tbody/tr | tr"/>
    <xsl:variable name="first-row" select="($header | $rows)[1]"/>
    <xsl:text>&#10;&#10;#table(&#10;  columns: </xsl:text>
    <xsl:value-of select="count($first-row/th | $first-row/td)"/>
    <xsl:text>,&#10;</xsl:text>
    <xsl:if test="$header">
      <xsl:text>  table.header(</xsl:text>
      <xsl:apply-templates select="$header/th | $header/td"/>
      <xsl:text>),&#10;</xsl:text>
    </xsl:if>
    <xsl:for-each select="$rows">
      <xsl:text>  </xsl:text>
      <xsl:apply-templates select="th | td"/>
      <xsl:text>&#10;</xsl:text>
    </xsl:for-each>
    <xsl:text>)&#10;</xsl:text>
  </xsl:template>

  <xsl:template match="th | td">
    <xsl:choose>
      <xsl:when test="@colspan or @rowspan">
        <xsl:text>table.cell(</xsl:text>
        <xsl:if test="@colspan">
          <xsl:text>colspan: </xsl:text><xsl:value-of select="@colspan"/>
        </xsl:if>
        <xsl:if test="@colspan and @rowspan"><xsl:text>, </xsl:text></xsl:if>
        <xsl:if test="@rowspan">
          <xsl:text>rowspan: </xsl:text><xsl:value-of select="@rowspan"/>
        </xsl:if>
        <xsl:text>)[</xsl:text>
      </xsl:when>
      <xsl:otherwise>
        <xsl:text>[</xsl:text>
      </xsl:otherwise>
    </xsl:choose>
    <xsl:apply-templates/>
    <xsl:text>], </xsl:text>
  </xsl:template>

  <!-- Inline formatting: function forms are robust to surrounding spaces.
       Every inline expression is terminated with ';' so that following text
       starting with '(' '[' or '.name' is never parsed by Typst as a call,
       content argument or field access on the expression result. -->
  <xsl:template match="b">
    <xsl:text>#strong[</xsl:text>
    <xsl:apply-templates/>
    <xsl:text>];</xsl:text>
  </xsl:template>

  <xsl:template match="i">
    <xsl:text>#emph[</xsl:text>
    <xsl:apply-templates/>
    <xsl:text>];</xsl:text>
  </xsl:template>

  <xsl:template match="code">
    <xsl:call-template name="raw-inline">
      <xsl:with-param name="text" select="."/>
    </xsl:call-template>
  </xsl:template>

  <!-- Inline raw: #raw("...") copes with backticks inside the text -->
  <xsl:template name="raw-inline">
    <xsl:param name="text"/>
    <xsl:text>#raw(</xsl:text>
    <xsl:call-template name="typst-string">
      <xsl:with-param name="text" select="$text"/>
    </xsl:call-template>
    <xsl:text>);</xsl:text>
  </xsl:template>

</xsl:stylesheet>
