//=============================================================================
// Copyright (c) 2016-2018 Allan CORNET (Nelson)
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#include "Markdown.hpp"
#include "characters_encoding.hpp"
#include <cstdio>
//=============================================================================
extern "C"
{
#include "document.h"
#include "html.h"
}
//=============================================================================
#define DEF_IUNIT 1024
#define DEF_OUNIT 64
#define DEF_MAX_NESTING 16
//=============================================================================
enum renderer_type
{
    RENDERER_HTML,
    RENDERER_HTML_TOC
};
//=============================================================================
struct option_data
{
    int done;

    /* time reporting */
    int show_time;

    /* I/O */
    size_t iunit;
    size_t ounit;
    const char* filename;

    /* renderer */
    enum renderer_type renderer;
    int toc_level;
    hoedown_html_flags html_flags;

    /* parsing */
    hoedown_extensions extensions;
    size_t max_nesting;
};
//=============================================================================
bool
Nelson::MarkdownFile(std::wstring inputMarkdownFilename, std::wstring outputHtmlFilename)
{
    hoedown_buffer *ib, *ob;
    hoedown_renderer* renderer = nullptr;
    void (*renderer_free)(hoedown_renderer*) = nullptr;
    hoedown_document* document;
    struct option_data data;
    data.done = 0;
    data.show_time = 0;
    data.iunit = DEF_IUNIT;
    data.ounit = DEF_OUNIT;
    data.filename = nullptr;
    data.renderer = RENDERER_HTML;
    data.toc_level = 0;
    data.html_flags = HOEDOWN_HTML_USE_XHTML;
    data.extensions
        = (hoedown_extensions)(HOEDOWN_EXT_MATH | HOEDOWN_EXT_TABLES | HOEDOWN_EXT_FENCED_CODE);
    data.max_nesting = DEF_MAX_NESTING;
#ifdef _MSCVER
    FILE* file = _wfopen(inputMarkdownFilename.c_str(), L"r");
#else
    FILE* file = fopen(wstring_to_utf8(inputMarkdownFilename).c_str(), "r");
#endif
    if (file == nullptr) {
        return false;
    }
    ib = hoedown_buffer_new(data.iunit);
    if (hoedown_buffer_putf(ib, file)) {
        return false;
    }
    fclose(file);
    switch (data.renderer) {
    case RENDERER_HTML:
        renderer = hoedown_html_renderer_new(data.html_flags, data.toc_level);
        renderer_free = hoedown_html_renderer_free;
        break;
    case RENDERER_HTML_TOC:
        renderer = hoedown_html_toc_renderer_new(data.toc_level);
        renderer_free = hoedown_html_renderer_free;
        break;
    };
    ob = hoedown_buffer_new(data.ounit);
    document = hoedown_document_new(renderer, data.extensions, data.max_nesting);
    hoedown_document_render(document, ob, ib->data, ib->size);
    hoedown_buffer_free(ib);
    hoedown_document_free(document);
    renderer_free(renderer);
#ifdef _MSCVER
    file = _wfopen(outputHtmlFilename.c_str(), L"wt");
#else
    file = fopen(wstring_to_utf8(outputHtmlFilename).c_str(), "wt");
#endif
    fwrite(ob->data, 1, ob->size, file);
    hoedown_buffer_free(ob);
    bool res = ferror(file) ? true : false;
    fclose(file);
    if (res) {
        return false;
    }
    return true;
}
//=============================================================================
bool
Nelson::MarkdownString(std::string inputMarkdownString, std::string& outputHtmlString)
{
    hoedown_buffer *ib, *ob;
    hoedown_renderer* renderer = nullptr;
    void (*renderer_free)(hoedown_renderer*) = nullptr;
    hoedown_document* document;
    struct option_data data;
    data.done = 0;
    data.show_time = 0;
    data.iunit = DEF_IUNIT;
    data.ounit = DEF_OUNIT;
    data.filename = nullptr;
    data.renderer = RENDERER_HTML;
    data.toc_level = 0;
    data.html_flags = HOEDOWN_HTML_USE_XHTML;
    data.extensions
        = (hoedown_extensions)(HOEDOWN_EXT_MATH | HOEDOWN_EXT_TABLES | HOEDOWN_EXT_FENCED_CODE);
    data.max_nesting = DEF_MAX_NESTING;
    std::string UTF8 = inputMarkdownString;
    ib = hoedown_buffer_new(UTF8.size());
    hoedown_buffer_puts(ib, UTF8.c_str());
    switch (data.renderer) {
    case RENDERER_HTML:
        renderer = hoedown_html_renderer_new(data.html_flags, data.toc_level);
        renderer_free = hoedown_html_renderer_free;
        break;
    case RENDERER_HTML_TOC:
        renderer = hoedown_html_toc_renderer_new(data.toc_level);
        renderer_free = hoedown_html_renderer_free;
        break;
    };
    ob = hoedown_buffer_new(data.ounit);
    document = hoedown_document_new(renderer, data.extensions, data.max_nesting);
    hoedown_document_render(document, ob, ib->data, ib->size);
    hoedown_buffer_free(ib);
    hoedown_document_free(document);
    renderer_free(renderer);
    std::string strOut;
    for (size_t k = 0; k < ob->size; k++) {
        strOut.push_back(ob->data[k]);
    }
    hoedown_buffer_free(ob);
    outputHtmlString = strOut;
    return true;
}
//=============================================================================
bool
Nelson::MarkdownString(std::wstring inputMarkdownString, std::wstring& outputHtmlString)
{
    std::string UTF8 = wstring_to_utf8(inputMarkdownString);
    std::string strOut = "";
    bool res = MarkdownString(UTF8, strOut);
    outputHtmlString = utf8_to_wstring(strOut);
    return res;
}
//=============================================================================