//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "XmlDocParamOutput.hpp"
#include "HtmlTags.hpp"
#include "XmlDocumentTags.hpp"
#include "characters_encoding.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
XmlDocParamOutput::XmlDocParamOutput() { paramOutputItemVector.clear(); }
//=============================================================================
XmlDocParamOutput::~XmlDocParamOutput()
{
    for (auto& k : paramOutputItemVector) {
        delete k;
        k = nullptr;
    }
    paramOutputItemVector.clear();
}
//=============================================================================
void
XmlDocParamOutput::append(const std::wstring& name, const std::wstring& description)
{
    XmlDocParamOutputItem* item = nullptr;
    try {
        item = new XmlDocParamOutputItem(name, description);
    } catch (const std::bad_alloc&) {
        item = nullptr;
    }
    if (item) {
        paramOutputItemVector.push_back(item);
    }
}
//=============================================================================
std::wstring
XmlDocParamOutput::getItemType()
{
    return utf8_to_wstring(PARAM_OUTPUT_TAG);
}
//=============================================================================
bool
XmlDocParamOutput::writeAsHtml(std::string& utf8stream)
{
    utf8stream = utf8stream + HTML_H3_IN_TAG + _("Output argument") + HTML_H3_OUT_TAG + "\n";
    utf8stream = utf8stream + HTML_HR_OUT_TAG + "\n";
    utf8stream = utf8stream + "\n";
    utf8stream = utf8stream + HTML_DL_IN_TAG + "\n";
    for (auto& k : paramOutputItemVector) {
        k->writeAsHtml(utf8stream);
    }
    utf8stream = utf8stream + HTML_DL_OUT_TAG + "\n";
    utf8stream = utf8stream + "\n";
    return true;
}
//=============================================================================
bool
XmlDocParamOutput::writeAsMarkdown(std::string& utf8stream)
{
    utf8stream = utf8stream + "## " + _("Output argument") + "\n";
    utf8stream = utf8stream + "\n";
    for (auto& k : paramOutputItemVector) {
        k->writeAsMarkdown(utf8stream);
    }
    utf8stream = utf8stream + "\n";
    return true;
}
//=============================================================================
}
//=============================================================================
