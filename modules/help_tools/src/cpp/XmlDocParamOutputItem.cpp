//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "XmlDocParamOutputItem.hpp"
#include "HtmlTags.hpp"
#include "XmlDocumentTags.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
XmlDocParamOutputItem::XmlDocParamOutputItem(
    const std::wstring& name, const std::wstring& description)
{
    this->_name.assign(name);
    this->_description.assign(description);
}
//=============================================================================
XmlDocParamOutputItem::~XmlDocParamOutputItem()
{
    this->_name.clear();
    this->_description.clear();
}
//=============================================================================
std::wstring
XmlDocParamOutputItem::getName()
{
    return this->_name;
}
//=============================================================================
std::wstring
XmlDocParamOutputItem::getDescription()
{
    return this->_description;
}
//=============================================================================
std::wstring
XmlDocParamOutputItem::getItemType()
{
    return utf8_to_wstring(PARAM_OUTPUT_TAG);
}
//=============================================================================
bool
XmlDocParamOutputItem::writeAsHtml(std::string& utf8stream)
{
    utf8stream = utf8stream + HTML_DT_IN_TAG + "<span class=\"term\">"
        + wstring_to_utf8(this->_name) + "</span>" + HTML_DT_OUT_TAG + "\n";
    utf8stream = utf8stream + HTML_DD_IN_TAG + "\n";
    utf8stream = utf8stream + "<p class=\"para\">" + wstring_to_utf8(this->_description)
        + HTML_P_OUT_TAG + "\n";
    utf8stream = utf8stream + HTML_DD_OUT_TAG + "\n";
    utf8stream = utf8stream + "\n";
    return true;
}
//=============================================================================
bool
XmlDocParamOutputItem::writeAsMarkdown(std::string& utf8stream)
{
    utf8stream = utf8stream + " - " + wstring_to_utf8(this->_name);
    utf8stream = utf8stream + " - " + wstring_to_utf8(this->_description) + "\n";
    return true;
}
//=============================================================================
}
//=============================================================================
