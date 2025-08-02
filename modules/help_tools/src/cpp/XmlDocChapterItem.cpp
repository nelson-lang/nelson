//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "XmlDocChapterItem.hpp"

#include <utility>
#include "HtmlTags.hpp"
#include "XmlDocumentTags.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
XmlDocChapterItem::XmlDocChapterItem(std::wstring chapter) : _chapter(std::move(chapter)) { }
//=============================================================================
XmlDocChapterItem::~XmlDocChapterItem() { this->_chapter.clear(); }
//=============================================================================
void
XmlDocChapterItem::setValue(const std::wstring& value)
{
    this->_chapter = value;
}
//=============================================================================
std::wstring
XmlDocChapterItem::getValue()
{
    return this->_chapter;
}
//=============================================================================
std::wstring
XmlDocChapterItem::getItemType()
{
    return utf8_to_wstring(CHAPTER_TAG);
}
//=============================================================================
bool
XmlDocChapterItem::writeHeaderAsHtml(std::string& utf8stream)
{
    utf8stream = utf8stream + "\t"
        + R"(<meta http-equiv="Content-Type" content="text/html; charset=utf-8" />)" + "\n";
    utf8stream = utf8stream + "\t" + HTML_TITLE_IN_TAG + wstring_to_utf8(_chapter)
        + HTML_TITLE_OUT_TAG + "\n";
    return true;
}
//=============================================================================
bool
XmlDocChapterItem::writeAsHtml(std::string& utf8stream)
{
    utf8stream = utf8stream + HTML_H3_IN_TAG + wstring_to_utf8(_chapter) + HTML_H3_OUT_TAG + "\n";
    utf8stream = utf8stream + HTML_HR_OUT_TAG + "\n";
    utf8stream = utf8stream + "\n";
    return true;
}
//=============================================================================
bool
XmlDocChapterItem::writeHeaderAsMarkdown(std::string& utf8stream)
{
    utf8stream = utf8stream + "\n";
    utf8stream = utf8stream + "# " + wstring_to_utf8(_chapter) + "\n";
    return true;
}
//=============================================================================
bool
XmlDocChapterItem::writeAsMarkdown(std::string& utf8stream)
{
    utf8stream = utf8stream + wstring_to_utf8(_chapter) + "\n";
    utf8stream = utf8stream + "\n";
    return true;
}
//=============================================================================
}
//=============================================================================
