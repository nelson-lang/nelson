//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#ifdef _MSC_VER
#define _CRT_SECURE_NO_WARNINGS
#endif
//=============================================================================
#include "Markdown.hpp"
#include "characters_encoding.hpp"
#include <cstdio>
#include <cmark.h>
#include <fstream>
//=============================================================================
bool
Nelson::MarkdownFile(const std::wstring& inputMarkdownFilename,
    const std::wstring& outputHtmlFilename, MarkdownMode mode)
{
    std::ifstream infile;
#if _MSC_VER
    infile.open(inputMarkdownFilename, std::ios::in | std::ios::binary);
#else
    infile.open(wstring_to_utf8(inputMarkdownFilename), std::ios::in | std::ios::binary);
#endif
    if (!infile.is_open()) {
        return false;
    }
    std::string markdownContent(
        (std::istreambuf_iterator<char>(infile)), std::istreambuf_iterator<char>());
    infile.close();
    std::string outputHtmlString;
    if (!MarkdownString(markdownContent, outputHtmlString, mode)) {
        return false;
    }

    std::ofstream outfile;
#if _MSC_VER
    outfile.open(outputHtmlFilename, std::ios::out | std::ios::binary);
#else
    outfile.open(wstring_to_utf8(outputHtmlFilename), std::ios::out | std::ios::binary);
#endif
    if (!outfile.is_open()) {
        return false;
    }
    outfile.write(outputHtmlString.c_str(), outputHtmlString.size());
    outfile.close();
    return true;
}
//=============================================================================
bool
Nelson::MarkdownString(
    const std::string& inputMarkdownString, std::string& outputHtmlString, MarkdownMode mode)
{
    const char* markdown = inputMarkdownString.c_str();

    int options = CMARK_OPT_SAFE;
    if (mode == MarkdownMode::ADVANCED) {
        options = CMARK_OPT_SMART /* curly quotes, en/em dashes, ellipses */
            | CMARK_OPT_UNSAFE; /* allow raw HTML and unsafe URLs */
    }

    cmark_node* doc = cmark_parse_document(markdown, inputMarkdownString.length(), options);
    if (!doc) {
        return false;
    }

    char* html = cmark_render_html(doc, options);

    cmark_node_free(doc);

    if (!html) {
        return false;
    }

    outputHtmlString = html;

    free(html);

    return true;
}
//=============================================================================
bool
Nelson::MarkdownString(
    const std::wstring& inputMarkdownString, std::wstring& outputHtmlString, MarkdownMode mode)
{
    std::string UTF8 = wstring_to_utf8(inputMarkdownString);
    std::string strOut = "";
    bool res = MarkdownString(UTF8, strOut, mode);
    outputHtmlString = utf8_to_wstring(strOut);
    return res;
}
//=============================================================================
