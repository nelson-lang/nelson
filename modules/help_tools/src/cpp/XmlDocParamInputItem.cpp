//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "XmlDocParamInputItem.hpp"
#include "HtmlTags.hpp"
#include "XmlDocumentTags.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
XmlDocParamInputItem::XmlDocParamInputItem(
    const std::wstring& name, const std::wstring& description)
{
    this->_name.assign(name);
    this->_description.assign(description);
}
//=============================================================================
XmlDocParamInputItem::~XmlDocParamInputItem()
{
    this->_name.clear();
    this->_description.clear();
}
//=============================================================================
std::wstring
XmlDocParamInputItem::getName()
{
    return this->_name;
}
//=============================================================================
std::wstring
XmlDocParamInputItem::getDescription()
{
    return this->_description;
}
//=============================================================================
std::wstring
XmlDocParamInputItem::getItemType()
{
    return utf8_to_wstring(PARAM_INPUT_TAG);
}
//=============================================================================
bool
XmlDocParamInputItem::writeAsHtml(std::string& utf8stream)
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
XmlDocParamInputItem::writeAsMarkdown(std::string& utf8stream)
{
    utf8stream = utf8stream + " - " + wstring_to_utf8(this->_name);
    utf8stream = utf8stream + " - " + wstring_to_utf8(this->_description) + "\n";
    return true;
}
//=============================================================================
}
//=============================================================================
