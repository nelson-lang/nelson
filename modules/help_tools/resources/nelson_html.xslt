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
    <html lang="en">
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
                     <script type="text/javascript">
                document.addEventListener('click', function(e) {
                  var a = (typeof e.target.closest === 'function') ? e.target.closest('a[href]') : null;
                  if (!a) return;
                  var href = a.getAttribute('href');
                  if (/^https?:\/\//i.test(href)) {
                    e.preventDefault();
                    window.open(href, '_blank', 'noopener');
                  }
                }, true);
            </script>
   
            <script src="highlight.pack.js"></script>
            <script>hljs.highlightAll();</script>
            <script>
                document.addEventListener('DOMContentLoaded', function() {
                    document.querySelectorAll('code.matlab').forEach(function(block) {
                        if (window.hljs) hljs.highlightElement(block);
                    });
                });
            </script>
            <script type="text/javascript">
                function copyExample(btn) {
                    var codeBlock = btn.parentNode.parentNode.querySelector('code');
                    if (codeBlock) {
                        var text = codeBlock.textContent;
                        navigator.clipboard.writeText(text).then(function() {
                            btn.setAttribute('data-copied', 'true');
                            btn.setAttribute('aria-label', 'Copied!');
                            btn.title = 'Copied!';
                            setTimeout(function(){ 
                                btn.removeAttribute('data-copied');
                                btn.setAttribute('aria-label', 'Copy');
                                btn.title = 'Copy';
                            }, 1200);
                        }, function() {
                            btn.setAttribute('aria-label', 'Error');
                            btn.title = 'Error';
                        });
                    }
                }
            </script>
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
                    <div class="section-title"><span class="syntax-icon">👤</span> Author<xsl:if test="count(xmldoc/authors/author_item) &gt; 1">s</xsl:if></div>
                    <div>
                        <xsl:for-each select="xmldoc/authors/author_item">
                            <xsl:value-of select="."/>
                            <xsl:if test="position() != last()">, </xsl:if>
                        </xsl:for-each>
                    </div>
                </div>
            </xsl:if>
        </body>
    </html>
    </xsl:template>

    <xsl:template match="xmldoc">

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
                    <div class="subtitle" style="padding-left:32px;">
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
                        <div class="section-title"><span class="syntax-icon">📖</span> Contents</div>
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
                    <span class="syntax-icon">📝</span> Syntax
                </div>
                <pre class="syntax-block">
<xsl:for-each select="syntax/syntax_item">
<xsl:value-of select="."/><xsl:text>
</xsl:text>
</xsl:for-each>
                </pre>
            </div>
        </xsl:if>

        <!-- Input Parameters Section -->
        <xsl:if test="param_input">
            <div class="section">
                <div class="section-title"><span class="syntax-icon">📥</span> Input Arguments</div>
                <table class="param-table">
                    <tr>
                        <th>Parameter</th>
                        <th>Description</th>
                    </tr>
                    <xsl:for-each select="param_input/param_input_item">
                        <tr>
                            <td class="param-name"><xsl:value-of select="param_name"/></td>
                            <td><xsl:value-of select="param_description"/></td>
                        </tr>
                    </xsl:for-each>
                </table>
            </div>
        </xsl:if>

        <!-- Output Parameters Section -->
        <xsl:if test="param_output">
            <div class="section">
                <div class="section-title"><span class="syntax-icon">📤</span> Output Arguments</div>
                <table class="param-table">
                    <tr>
                        <th>Parameter</th>
                        <th>Description</th>
                    </tr>
                    <xsl:for-each select="param_output/param_output_item">
                        <tr>
                            <td class="param-name"><xsl:value-of select="param_name"/></td>
                            <td><xsl:value-of select="param_description"/></td>
                        </tr>
                    </xsl:for-each>
                </table>
            </div>
        </xsl:if>

        <!-- Description Section -->
        <xsl:if test="description">
            <div class="section">
                <div class="section-title"><span class="syntax-icon">📄</span> Description</div>
                <div>
                    <xsl:apply-templates select="description/node()"/>
                </div>
            </div>
        </xsl:if>

        <!-- Examples Section with Copy Button and MATLAB syntax highlighting -->
        <xsl:if test="examples">
            <div class="section">
                <div class="section-title"><span class="syntax-icon">💡</span> Examples</div>
                <xsl:for-each select="examples/example_item">
                    <div class="example">
                        <xsl:if test="normalize-space(example_item_description)">
                            <div class="example-title"><xsl:value-of select="example_item_description"/></div>
                        </xsl:if>
                        <xsl:if test="example_item_data">
                            <div style="display:flex;align-items:flex-start;gap:8px;">
                                <code class="matlab syntax-block" style="flex:1 1 auto;"><xsl:call-template name="trim-example-lines"><xsl:with-param name="text" select="example_item_data"/></xsl:call-template></code>
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
                                    <img src="{$imgsrc}" align="{example_item_img/@align}" alt="Example illustration" style="width:100%;height:auto;display:block;margin:12px 0;background:#fff;border:1px solid #e2e8f0;border-radius:4px;box-shadow:0 1px 4px rgba(44,82,130,0.07);object-fit:contain;"/>
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
                <div class="section-title"><span class="syntax-icon">🔗</span> See Also</div>
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
                <div class="section-title"><span class="syntax-icon">📚</span> Bibliography</div>
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
                <div class="section-title"><span class="syntax-icon">🕔</span> Version History</div>
                <table class="param-table">
                    <tr>
                        <th>Version</th>
                        <th>Description</th>
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
<!-- Render <p> as HTML paragraph -->
<xsl:template match="p">
    <p><xsl:apply-templates/></p>
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
