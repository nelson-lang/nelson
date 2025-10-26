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
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:ext="http://io.github.nelson_lang/ext" version="1.0">
    <xsl:output method="html" encoding="UTF-8" indent="yes" doctype-public="-//W3C//DTD HTML 4.01//EN"/>

    <xsl:template match="/">
    <!-- Nelson Documentation XSLT Stylesheet -->
    <html>
        <xsl:attribute name="lang">
            <xsl:choose>
                <!-- en_US or zh_CN style -->
                <xsl:when test="contains(xmldoc/language, '_')">
                    <xsl:value-of select="substring-before(xmldoc/language, '_')"/>
                </xsl:when>
                <!-- en-US style -->
                <xsl:when test="contains(xmldoc/language, '-')">
                    <xsl:value-of select="substring-before(xmldoc/language, '-')"/>
                </xsl:when>
                <!-- bare language code like "en" or "fr" -->
                <xsl:when test="normalize-space(xmldoc/language)">
                    <xsl:value-of select="xmldoc/language"/>
                </xsl:when>
                <!-- default -->
                <xsl:otherwise>en</xsl:otherwise>
            </xsl:choose>
        </xsl:attribute>
        <head>
            <title>
                <xsl:choose>
                    <xsl:when test="xmldoc/keyword">
                        <xsl:value-of select="xmldoc/keyword"/>
                    </xsl:when>
                    <xsl:when test="xmldoc/chapter">
                        <xsl:value-of select="xmldoc/chapter"/>
                    </xsl:when>
                    <xsl:otherwise>Documentation</xsl:otherwise>
                </xsl:choose>
            </title>
            <link rel="stylesheet" href="highlight.css"/>
            <link rel="stylesheet" href="nelson_common.css"/>
            <script src="nelson_help.js"></script>

            <!-- Add dark/light prefers-color-scheme rules to make chapter descriptions readable on both themes -->
            <style>
                @media (prefers-color-scheme: dark) {
                    /* target both class names used in different XSLT outputs */
                    .chapter-description, .chapter-desc { color: #e6eef8 !important; }
                    .chapter-description p, .chapter-desc p { color: inherit !important; }
                }
                @media (prefers-color-scheme: light) {
                    .chapter-description, .chapter-desc { color: #444 !important; }
                    .chapter-description p, .chapter-desc p { color: inherit !important; }
                }
            </style>
        </head>
        <body>
            <!-- Help Summary Button at top left -->
            <a href="./index.html" class="home-summary-link">
                <button class="home-summary-btn" aria-label="Back to help index">
                    <svg xmlns="http://www.w3.org/2000/svg" width="22" height="22" viewBox="0 0 24 24" fill="none" stroke="white" stroke-width="2" stroke-linecap="round" stroke-linejoin="round">
                        <path d="M3 12L12 3l9 9"/>
                        <path d="M9 21V12h6v9"/>
                        <path d="M9 21h6"/>
                    </svg>
                </button>
            </a>
            <xsl:apply-templates select="xmldoc"/>

            <!-- Author section always LAST -->
            <xsl:if test="xmldoc/authors/author_item">
                <div class="section" style="margin-top:30px;">
                    <div class="section-title"><span class="syntax-icon">👤</span> <xsl:choose>
                        <xsl:when test="xmldoc/language = 'fr_FR'">Auteur</xsl:when>
                        <xsl:otherwise>Author</xsl:otherwise>
                    </xsl:choose><xsl:if test="count(xmldoc/authors/author_item) &gt; 1">s</xsl:if></div>
                    <div>
                        <xsl:for-each select="xmldoc/authors/author_item">
                            <xsl:value-of select="."/>
                            <xsl:if test="position() != last()">, </xsl:if>
                        </xsl:for-each>
                    </div>
                </div>
            </xsl:if>

            <!-- GitHub Edit Button: placed as the last element on the page -->
            <div class="section" style="margin-top:40px; text-align:center;">
                <a id="github-edit-link" class="github-edit-btn" target="_blank" rel="noopener noreferrer">
                    <svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round">
                        <path d="M12 20h9"></path>
                        <path d="M16.5 3.5a2.121 2.121 0 0 1 3 3L7 19l-4 1 1-4L16.5 3.5z"></path>
                    </svg>
                    <span id="github-edit-text"><xsl:choose>
                        <xsl:when test="xmldoc/language = 'fr_FR'">Modifier cette page sur GitHub</xsl:when>
                        <xsl:otherwise>Edit this page on GitHub</xsl:otherwise>
                    </xsl:choose></span>
                </a>
                <style>
                    .github-edit-btn { display:inline-flex; align-items:center; gap:6px; font-size:14px; padding:6px 12px; border:1px solid #ddd; border-radius:4px; background:#f5f5f5; color:#333; text-decoration:none; transition:all 0.2s ease; }
                    .github-edit-btn:hover { background:#e9e9e9; border-color:#ccc; color:#000; }
                </style>
            </div>

            <!-- Include consolidated help JS -->
        </body>
    </html>
    </xsl:template>

    <xsl:template match="xmldoc">

        <xsl:variable name="syntax-text">
            <xsl:choose>
                <xsl:when test="language = 'fr_FR'">Syntaxe</xsl:when>
                <xsl:otherwise>Syntax</xsl:otherwise>
            </xsl:choose>
        </xsl:variable>

        <xsl:variable name="contents-text">
            <xsl:choose>
                <xsl:when test="language = 'fr_FR'">Sommaire</xsl:when>
                <xsl:otherwise>Contents</xsl:otherwise>
            </xsl:choose>
        </xsl:variable>

        <xsl:variable name="input-arguments-text">
            <xsl:choose>
                <xsl:when test="language = 'fr_FR'">Arguments d'entrée</xsl:when>
                <xsl:otherwise>Input Arguments</xsl:otherwise>
            </xsl:choose>
        </xsl:variable>

        <xsl:variable name="output-arguments-text">
            <xsl:choose>
                <xsl:when test="language = 'fr_FR'">Arguments de sortie</xsl:when>
                <xsl:otherwise>Output Arguments</xsl:otherwise>
            </xsl:choose>
        </xsl:variable>

        <xsl:variable name="description-text">
            <xsl:choose>
                <xsl:when test="language = 'fr_FR'">Description</xsl:when>
                <xsl:otherwise>Description</xsl:otherwise>
            </xsl:choose>
        </xsl:variable>

        <xsl:variable name="examples-text">
            <xsl:choose>
                <xsl:when test="language = 'fr_FR'">Exemples</xsl:when>
                <xsl:otherwise>Examples</xsl:otherwise>
            </xsl:choose>
        </xsl:variable>

        <xsl:variable name="see-also-text">
            <xsl:choose>
                <xsl:when test="language = 'fr_FR'">Voir aussi</xsl:when>
                <xsl:otherwise>See Also</xsl:otherwise>
            </xsl:choose>
        </xsl:variable>

        <xsl:variable name="bibliography-text">
            <xsl:choose>
                <xsl:when test="language = 'fr_FR'">Bibliographie</xsl:when>
                <xsl:otherwise>Bibliography</xsl:otherwise>
            </xsl:choose>
        </xsl:variable>

        <xsl:variable name="version-history-text">
            <xsl:choose>
                <xsl:when test="language = 'fr_FR'">Historique des versions</xsl:when>
                <xsl:otherwise>Version History</xsl:otherwise>
            </xsl:choose>
        </xsl:variable>

        <xsl:variable name="author-text">
            <xsl:choose>
                <xsl:when test="language = 'fr_FR'">Auteur</xsl:when>
                <xsl:otherwise>Author</xsl:otherwise>
            </xsl:choose>
        </xsl:variable>

        <xsl:variable name="parameter-text">
            <xsl:choose>
                <xsl:when test="language = 'fr_FR'">Paramètre</xsl:when>
                <xsl:otherwise>Parameter</xsl:otherwise>
            </xsl:choose>
        </xsl:variable>

        <xsl:variable name="version-text">
            <xsl:choose>
                <xsl:when test="language = 'fr_FR'">Version</xsl:when>
                <xsl:otherwise>Version</xsl:otherwise>
            </xsl:choose>
        </xsl:variable>

        <!-- Render all children of xmldoc in order, with custom templates for known elements -->
        <xsl:for-each select="*">
            <xsl:choose>
                <xsl:when test="name() = 'keyword'">
                    <div class="header">
                        <div class="title">
                            <span class="keyword-title">
                                <xsl:value-of select="."/>
                            </span>
                            <xsl:for-each select="../keyword_alias">
                                 <xsl:text>, </xsl:text>
                                <span class="keyword-alias">
                                    <xsl:value-of select="."/>
                                </span>
                            </xsl:for-each>
                        </div>
                    </div>
                </xsl:when>
                <xsl:when test="name() = 'chapter'">
                    <div class="header">
                        <div class="title">
                            <xsl:value-of select="."/>
                        </div>
                    </div>
                </xsl:when>
                <xsl:when test="name() = 'short_description'">
                    <div class="section syntax-section">
                        <div class="subtitle"><xsl:value-of select="."/></div>
                    </div>
                </xsl:when>
                <xsl:when test="name() = 'chapter_description'">
                    <!-- add specific class for targeted styling -->
                    <div class="subtitle chapter-description" style="padding-left:32px;">
                        <xsl:choose>
                            <xsl:when test="p">
                                <xsl:for-each select="p">
                                    <p style="margin:2px 0 2px 0; padding:0; display:block;"> 
                                        <xsl:value-of select="."/>
                                    </p>
                                </xsl:for-each>
                            </xsl:when>
                            <xsl:otherwise>
                                <xsl:value-of select="."/>
                            </xsl:otherwise>
                        </xsl:choose>
                    </div>
                </xsl:when>
                <xsl:when test="name() = 'ul'">
                    <div class="section">
                        <div class="section-title"><span class="syntax-icon">📖</span> <xsl:value-of select="$contents-text"/></div>
                        <ul style="list-style-type:none; padding-left:0;">
                            <xsl:for-each select="li">
                                <li style="margin-bottom:8px;">
                                    <a href="{a/@href}" style="font-weight:600; color:#1976d2; text-decoration:none;">
                                        <xsl:value-of select="a"/>
                                    </a>
                                    <xsl:if test="span">
                                        <span style="color:#555; font-weight:400;">
                                            <xsl:value-of select="span"/>
                                        </span>
                                    </xsl:if>
                                </li>
                            </xsl:for-each>
                        </ul>
                    </div>
                </xsl:when>
                <!-- Default: do nothing for unknown elements here, handled by other templates -->
            </xsl:choose>
        </xsl:for-each>

    <!-- Syntax Section (pre block, modern look, line by line) -->
        <xsl:if test="syntax">
            <div class="section syntax-section">
                <div class="section-title">
                    <span class="syntax-icon">📝</span> <xsl:value-of select="$syntax-text"/>
                </div>
                <pre class="syntax-block"><code class="language-matlab">
<xsl:for-each select="syntax/syntax_item">
<xsl:value-of select="."/><xsl:text>
</xsl:text>
</xsl:for-each>
</code></pre>
            </div>
        </xsl:if>

        <!-- Input Parameters Section -->
        <xsl:if test="param_input">
            <div class="section">
                <div class="section-title"><span class="syntax-icon">📥</span> <xsl:value-of select="$input-arguments-text"/></div>
                <table class="param-table">
                    <tr>
                        <th><xsl:value-of select="$parameter-text"/></th>
                        <th><xsl:value-of select="$description-text"/></th>
                    </tr>
                    <xsl:for-each select="param_input/param_input_item">
                        <tr>
                            <td class="param-name"><xsl:value-of select="param_name"/></td>
                            <td><xsl:apply-templates select="param_description/node()"/></td>
                        </tr>
                    </xsl:for-each>
                </table>
            </div>
        </xsl:if>

        <!-- Output Parameters Section -->
        <xsl:if test="param_output">
            <div class="section">
                <div class="section-title"><span class="syntax-icon">📤</span> <xsl:value-of select="$output-arguments-text"/></div>
                <table class="param-table">
                    <tr>
                        <th><xsl:value-of select="$parameter-text"/></th>
                        <th><xsl:value-of select="$description-text"/></th>
                    </tr>
                    <xsl:for-each select="param_output/param_output_item">
                        <tr>
                            <td class="param-name"><xsl:value-of select="param_name"/></td>
                            <td><xsl:apply-templates select="param_description/node()"/></td>
                        </tr>
                    </xsl:for-each>
                </table>
            </div>
        </xsl:if>

        <!-- Description Section -->
        <xsl:if test="description">
            <div class="section">
                <div class="section-title"><span class="syntax-icon">📄</span> <xsl:value-of select="$description-text"/></div>
                <div>
                    <xsl:apply-templates select="description/node()"/>
                </div>
            </div>
        </xsl:if>

        <!-- Examples Section with Copy Button and MATLAB syntax highlighting -->
        <xsl:if test="examples">
            <div class="section">
                <div class="section-title"><span class="syntax-icon">💡</span> <xsl:value-of select="$examples-text"/></div>
                <xsl:for-each select="examples/example_item">
                    <div class="example">
                        <xsl:if test="normalize-space(example_item_description)">
                            <div class="example-title"><xsl:value-of select="example_item_description"/></div>
                        </xsl:if>
                        <xsl:if test="example_item_data">
                            <div style="display:flex;align-items:flex-start;gap:8px;">
                                <pre class="syntax-block" style="flex:1 1 auto;margin:0;"><code class="language-matlab"><xsl:call-template name="trim-example-lines"><xsl:with-param name="text" select="example_item_data"/></xsl:call-template></code></pre>
                                <button class="copy-btn" type="button" onclick="copyExample(this)" title="Copy">
                                    <svg xmlns="http://www.w3.org/2000/svg" width="18" height="18" viewBox="0 0 20 20" fill="none" style="display:block;">
                                        <rect x="7" y="3" width="10" height="14" rx="2" fill="white" fill-opacity="0.15" stroke="white" stroke-width="1.2"/>
                                        <rect x="3" y="7" width="10" height="10" rx="2" fill="currentColor"/>
                                    </svg>
                                </button>
                            </div>
                        </xsl:if>
                        <xsl:if test="example_item_img">
                            <xsl:variable name="imgsrc" select="ext:copy_img(example_item_img/@src)"/>
                            <xsl:choose>
                                <xsl:when test="substring($imgsrc, string-length($imgsrc) - 3) = '.svg'">
                                    <img src="{$imgsrc}" align="{example_item_img/@align}" alt="Example illustration" style="width:960px;height:580px;display:block;margin:12px auto;background:#fff;border:1px solid #e2e8f0;border-radius:4px;box-shadow:0 1px 4px rgba(44,82,130,0.07);object-fit:contain;"/>
                                </xsl:when>
                                <xsl:otherwise>
                                    <img src="{$imgsrc}" align="{example_item_img/@align}" alt="Example illustration" style="width:100%;height:auto;display:block;margin:12px 0;background:#f9fafb;border:1px solid #e2e8f0;border-radius:4px;box-shadow:0 1px 4px rgba(44,82,130,0.07);object-fit:contain;"/>
                                </xsl:otherwise>
                            </xsl:choose>
                        </xsl:if>
                    </div>
                </xsl:for-each>
            </div>
        </xsl:if>

        <!-- See Also Section -->
        <xsl:if test="see_also">
            <div class="section">
                <div class="section-title"><span class="syntax-icon">🔗</span> <xsl:value-of select="$see-also-text"/></div>
                <div>
                    <xsl:for-each select="see_also/see_also_item">
                        <a class="see-also-link">
                          <xsl:attribute name="href">
                            <xsl:value-of select="ext:replace(link/@linkend)"/>
                          </xsl:attribute>
                          <xsl:value-of select="link"/>
                        </a>
                    </xsl:for-each>
                </div>
            </div>
        </xsl:if>

        <!-- Used Function Section -->
        <xsl:if test="used_function[text()]">
            <div class="section">
                <div class="section-title">Used Functions</div>
                <div><xsl:value-of select="used_function"/></div>
            </div>
        </xsl:if>

        <!-- Bibliography Section: URL detection -->
        <xsl:if test="bibliography and normalize-space(bibliography)">
            <div class="section">
                <div class="section-title"><span class="syntax-icon">📚</span> <xsl:value-of select="$bibliography-text"/></div>
                <div>
                    <xsl:choose>
                        <xsl:when test="starts-with(bibliography, 'http://') or starts-with(bibliography, 'https://')">
                            <a class="bibliography-link" href="{bibliography}" target="_blank">
                                <xsl:value-of select="bibliography"/>
                            </a>
                        </xsl:when>
                        <xsl:otherwise>
                            <xsl:value-of select="bibliography"/>
                        </xsl:otherwise>
                    </xsl:choose>
                </div>
            </div>
        </xsl:if>

        <!-- History Section -->
        <xsl:if test="history">
            <div class="section">
                <div class="section-title"><span class="syntax-icon">🕔</span> <xsl:value-of select="$version-history-text"/></div>
                <table class="param-table">
                    <tr>
                        <th><xsl:value-of select="$version-text"/></th>
                        <th><xsl:value-of select="$description-text"/></th>
                    </tr>
                    <xsl:for-each select="history/history_item">
                        <tr>
                            <td class="version"><xsl:value-of select="history_version"/></td>
                            <td><xsl:value-of select="history_description"/></td>
                        </tr>
                    </xsl:for-each>
                </table>
            </div>
        </xsl:if>
    </xsl:template>

<!-- Render <b> as HTML bold -->
<xsl:template match="b">
    <b><xsl:apply-templates/></b>
</xsl:template>
<!-- Render <a> as HTML anchor -->
<xsl:template match="a">
    <a>
        <xsl:attribute name="href">
            <xsl:value-of select="@href"/>
        </xsl:attribute>
        <xsl:apply-templates/>
    </a>
</xsl:template>
<!-- Render inline <link> as an HTML anchor using linkend -->
<xsl:template match="link">
    <xsl:variable name="linkend" select="@linkend"/>
    <xsl:variable name="module">
        <xsl:if test="contains($linkend, '{') and contains($linkend, '}')">
            <xsl:value-of select="substring-before(substring-after($linkend, '{'), '}')"/>
        </xsl:if>
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
    <a>
        <xsl:attribute name="href">
            <xsl:choose>
                <xsl:when test="string-length($module) &gt; 0">
                    <xsl:text>../</xsl:text><xsl:value-of select="$module"/><xsl:text>/</xsl:text><xsl:value-of select="$function"/><xsl:text>.html</xsl:text>
                </xsl:when>
                <xsl:otherwise>
                    <xsl:value-of select="$function"/><xsl:text>.html</xsl:text>
                </xsl:otherwise>
            </xsl:choose>
        </xsl:attribute>
        <xsl:apply-templates/>
    </a>
</xsl:template>
<!-- Render <p> as HTML paragraph -->
<xsl:template match="p">
    <p><xsl:apply-templates/></p>
</xsl:template>

<xsl:template match="img">
  <xsl:variable name="imgsrc" select="ext:copy_img(@src)"/>
  <xsl:choose>
    <!-- Special styling for SVG (keeps large fixed view) -->
    <xsl:when test="substring($imgsrc, string-length($imgsrc) - 3) = '.svg'">
      <img>
        <xsl:attribute name="src"><xsl:value-of select="$imgsrc"/></xsl:attribute>
        <xsl:if test="@align"><xsl:attribute name="align"><xsl:value-of select="@align"/></xsl:attribute></xsl:if>
        <xsl:if test="@alt"><xsl:attribute name="alt"><xsl:value-of select="@alt"/></xsl:attribute></xsl:if>
        <xsl:if test="@width"><xsl:attribute name="width"><xsl:value-of select="@width"/></xsl:attribute></xsl:if>
        <xsl:if test="@height"><xsl:attribute name="height"><xsl:value-of select="@height"/></xsl:attribute></xsl:if>
        <xsl:attribute name="style">width:960px;height:580px;display:block;margin:12px auto;background:#fff;border:1px solid #e2e8f0;border-radius:4px;box-shadow:0 1px 4px rgba(44,82,130,0.07);object-fit:contain;</xsl:attribute>
      </img>
    </xsl:when>
    <xsl:otherwise>
      <img>
        <xsl:attribute name="src"><xsl:value-of select="$imgsrc"/></xsl:attribute>
        <xsl:if test="@align"><xsl:attribute name="align"><xsl:value-of select="@align"/></xsl:attribute></xsl:if>
        <xsl:if test="@alt"><xsl:attribute name="alt"><xsl:value-of select="@alt"/></xsl:attribute></xsl:if>
        <xsl:if test="@width"><xsl:attribute name="width"><xsl:value-of select="@width"/></xsl:attribute></xsl:if>
        <xsl:if test="@height"><xsl:attribute name="height"><xsl:value-of select="@height"/></xsl:attribute></xsl:if>
        <xsl:attribute name="style">width:100%;height:auto;display:block;margin:12px 0;background:#f9fafb;border:1px solid #e2e8f0;border-radius:4px;box-shadow:0 1px 4px rgba(44,82,130,0.07);object-fit:contain;</xsl:attribute>
      </img>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>
<!-- Render <ul> as HTML unordered list -->
<xsl:template match="ul">
    <ul>
        <xsl:apply-templates/>
    </ul>
</xsl:template>   

<!-- Render <li> as HTML list item -->
<xsl:template match="li">
    <li>
        <xsl:apply-templates/>
    </li>
</xsl:template>

<!-- Render <table> as HTML table -->
<xsl:template match="table">
    <table>
        <xsl:copy-of select="@*"/>
        <xsl:apply-templates/>
    </table>
</xsl:template>

<!-- Render <tr> as HTML table row -->
<xsl:template match="tr">
    <tr>
        <xsl:copy-of select="@*"/>
        <xsl:apply-templates/>
    </tr>
</xsl:template>

<!-- Render <th> as HTML table header -->
<xsl:template match="th">
    <th>
        <xsl:copy-of select="@*"/>
        <xsl:apply-templates/>
    </th>
</xsl:template>

<!-- Render <td> as HTML table data -->
<xsl:template match="td">
    <td>
        <xsl:copy-of select="@*"/>
        <xsl:apply-templates/>
    </td>
</xsl:template>

<!-- Render <latex> as display math: emit visible $$...$$ inside a span so browsers show it before MathJax runs -->
<xsl:template match="latex">
    <span class="math display">$$<xsl:value-of select="normalize-space(.)"/>$$</span>
</xsl:template>

<xsl:template name="trim-example-lines">
  <xsl:param name="text"/>
  <!-- Remove carriage returns -->
  <xsl:variable name="noCR" select="translate($text, '&#13;', '')"/>
  <!-- Remove first blank line -->
  <xsl:variable name="noFirstBlank">
    <xsl:choose>
      <xsl:when test="starts-with($noCR, '&#10;')">
        <xsl:value-of select="substring($noCR, 2)"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="$noCR"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:variable>
  <!-- Remove all trailing blank lines (all trailing newlines) -->
  <xsl:variable name="trimmed">
    <xsl:variable name="val" select="$noFirstBlank"/>
    <xsl:choose>
      <xsl:when test="substring($val, string-length($val)) = '&#10;'">
        <xsl:call-template name="trim-example-lines">
          <xsl:with-param name="text" select="substring($val, 1, string-length($val)-1)"/>
        </xsl:call-template>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="$val"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:variable>
  <xsl:value-of select="$trimmed"/>
</xsl:template>
</xsl:stylesheet>
