//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "XmlDocKeywordItem.hpp"
#include "HtmlTags.hpp"
#include "XmlDocumentTags.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
XmlDocKeywordItem::XmlDocKeywordItem(const std::wstring& keyword)
{
    this->_keyword.assign(keyword);
}
//=============================================================================
XmlDocKeywordItem::~XmlDocKeywordItem() { this->_keyword.clear(); }
//=============================================================================
void
XmlDocKeywordItem::setValue(const std::wstring& value)
{
    this->_keyword.assign(value);
}
//=============================================================================
std::wstring
XmlDocKeywordItem::getValue()
{
    return this->_keyword;
}
//=============================================================================
std::wstring
XmlDocKeywordItem::getItemType()
{
    return utf8_to_wstring(KEYWORD_TAG);
}
//=============================================================================
bool
XmlDocKeywordItem::writeAsHtml(std::string& utf8stream)
{
    utf8stream
        = utf8stream + HTML_H1_REFNAME_IN_TAG + wstring_to_utf8(_keyword) + HTML_H1_OUT_TAG + "\n";
    utf8stream = utf8stream + HTML_HR_OUT_TAG + "\n";
    utf8stream = utf8stream + "\n";
    return true;
}
//=============================================================================
bool
XmlDocKeywordItem::writeHeaderAsHtml(std::string& utf8stream)
{
    utf8stream = utf8stream + "\t"
        + R"(<meta http-equiv="Content-Type" content = "text/html; charset=utf-8" />)" + "\n";
    utf8stream = utf8stream + "\t" + HTML_TITLE_IN_TAG + wstring_to_utf8(_keyword)
        + HTML_TITLE_OUT_TAG + "\n";
    utf8stream = utf8stream + "\n";
    return true;
}
//=============================================================================
bool
XmlDocKeywordItem::writeAsMarkdown(std::string& utf8stream)
{
    utf8stream = utf8stream + "# " + wstring_to_utf8(_keyword) + "\n";
    utf8stream = utf8stream + "\n";
    return true;
}
//=============================================================================
bool
XmlDocKeywordItem::writeHeaderAsMarkdown(std::string& utf8stream)
{
    return true;
}
//=============================================================================

}
//=============================================================================
