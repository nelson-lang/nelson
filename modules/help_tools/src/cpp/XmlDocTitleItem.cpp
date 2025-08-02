//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "XmlDocTitleItem.hpp"
#include "HtmlTags.hpp"
#include "XmlDocumentTags.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
XmlDocTitleItem::XmlDocTitleItem(const std::wstring& title) { this->_title.assign(title); }
//=============================================================================
XmlDocTitleItem::~XmlDocTitleItem() { this->_title.clear(); }
//=============================================================================
void
XmlDocTitleItem::setValue(const std::wstring& value)
{
    this->_title.assign(value);
}
//=============================================================================
std::wstring
XmlDocTitleItem::getValue()
{
    return this->_title;
}
//=============================================================================
std::wstring
XmlDocTitleItem::getItemType()
{
    return utf8_to_wstring(TITLE_TAG);
}
//=============================================================================
bool
XmlDocTitleItem::writeAsHtml(std::string& utf8stream)
{
    utf8stream = utf8stream + HTML_H1_REFNAME_IN_TAG + wstring_to_utf8(this->_title)
        + HTML_H1_OUT_TAG + "\n";
    utf8stream = utf8stream + HTML_HR_OUT_TAG + "\n";
    utf8stream = utf8stream + "\n";
    return true;
}
//=============================================================================
bool
XmlDocTitleItem::writeHeaderAsHtml(std::string& utf8stream)
{
    utf8stream = utf8stream + "\t"
        + R"(<meta http-equiv="Content-Type" content = "text/html; charset=utf-8" />)" + "\n";
    utf8stream = utf8stream + "\t" + HTML_TITLE_IN_TAG + wstring_to_utf8(this->_title)
        + HTML_TITLE_OUT_TAG + "\n";
    utf8stream = utf8stream + "\n";
    return true;
}
//=============================================================================
bool
XmlDocTitleItem::writeAsMarkdown(std::string& utf8stream)
{
    utf8stream = utf8stream + wstring_to_utf8(this->_title) + "\n";
    utf8stream = utf8stream + "\n";
    return true;
}
//=============================================================================
bool
XmlDocTitleItem::writeHeaderAsMarkdown(std::string& utf8stream)
{
    utf8stream = utf8stream + "\n";
    utf8stream = utf8stream + "# " + wstring_to_utf8(this->_title) + "\n";
    utf8stream = utf8stream + "\n";
    return true;
}
//=============================================================================
}
//=============================================================================
