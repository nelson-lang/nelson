//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "XmlDocUsedFunctionItem.hpp"
#include "HtmlTags.hpp"
#include "XmlDocumentTags.hpp"
#include "characters_encoding.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
XmlDocUsedFunctionItem::XmlDocUsedFunctionItem(const std::wstring& usedfunction)
{
    this->_usedfunction.assign(usedfunction);
}
//=============================================================================
XmlDocUsedFunctionItem::~XmlDocUsedFunctionItem() { this->_usedfunction.clear(); }
//=============================================================================
std::wstring
XmlDocUsedFunctionItem::getItemType()
{
    return utf8_to_wstring(USED_FUNCTION_TAG);
}
//=============================================================================
bool
XmlDocUsedFunctionItem::writeAsHtml(std::string& utf8stream)
{
    if (!this->_usedfunction.empty()) {
        utf8stream = utf8stream + HTML_H3_IN_TAG + _("Used function(s)") + HTML_H3_OUT_TAG + "\n";
        utf8stream = utf8stream + HTML_HR_OUT_TAG + "\n";
        utf8stream = utf8stream + "\n";
        utf8stream = utf8stream + wstring_to_utf8(this->_usedfunction) + "\n";
        utf8stream = utf8stream + "\n";
    }
    return true;
}
//=============================================================================
bool
XmlDocUsedFunctionItem::writeAsMarkdown(std::string& utf8stream)
{
    if (!this->_usedfunction.empty()) {
        utf8stream = utf8stream + _("Used function(s)") + "\n";
        utf8stream = utf8stream + "\n";
        utf8stream = utf8stream + wstring_to_utf8(this->_usedfunction) + "\n";
        utf8stream = utf8stream + "\n";
    }
    return true;
}
//=============================================================================
}
//=============================================================================
