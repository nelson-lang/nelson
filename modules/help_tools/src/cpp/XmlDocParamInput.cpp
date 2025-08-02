//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "XmlDocParamInput.hpp"
#include "HtmlTags.hpp"
#include "XmlDocumentTags.hpp"
#include "characters_encoding.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
XmlDocParamInput::XmlDocParamInput() { paramInputItemVector.clear(); }
//=============================================================================
XmlDocParamInput::~XmlDocParamInput()
{
    for (auto& k : paramInputItemVector) {
        delete k;
        k = nullptr;
    }
    paramInputItemVector.clear();
}
//=============================================================================
void
XmlDocParamInput::append(const std::wstring& name, const std::wstring& description)
{
    XmlDocParamInputItem* item = nullptr;
    try {
        item = new XmlDocParamInputItem(name, description);
    } catch (const std::bad_alloc&) {
        item = nullptr;
    }
    if (item) {
        paramInputItemVector.push_back(item);
    }
}
//=============================================================================
std::wstring
XmlDocParamInput::getItemType()
{
    return utf8_to_wstring(PARAM_INPUT_TAG);
}
//=============================================================================
bool
XmlDocParamInput::writeAsHtml(std::string& utf8stream)
{
    utf8stream = utf8stream + HTML_H3_IN_TAG + _("Input argument") + HTML_H3_OUT_TAG + "\n";
    utf8stream = utf8stream + HTML_HR_OUT_TAG + "\n";
    utf8stream = utf8stream + "\n";
    utf8stream = utf8stream + HTML_DL_IN_TAG + "\n";
    for (auto& k : paramInputItemVector) {
        k->writeAsHtml(utf8stream);
    }
    utf8stream = utf8stream + HTML_DL_OUT_TAG + "\n";
    utf8stream = utf8stream + "\n";
    return true;
}
//=============================================================================
bool
XmlDocParamInput::writeAsMarkdown(std::string& utf8stream)
{
    utf8stream = utf8stream + "## " + _("Input argument") + "\n";
    utf8stream = utf8stream + "\n";
    for (auto& k : paramInputItemVector) {
        k->writeAsMarkdown(utf8stream);
    }
    utf8stream = utf8stream + "\n";
    return true;
}
//=============================================================================
}
//=============================================================================
