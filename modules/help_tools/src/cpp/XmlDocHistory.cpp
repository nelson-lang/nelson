//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "XmlDocHistory.hpp"
#include "HtmlTags.hpp"
#include "XmlDocumentTags.hpp"
#include "characters_encoding.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
XmlDocHistory::XmlDocHistory() { historyVector.clear(); }
//=============================================================================
XmlDocHistory::~XmlDocHistory()
{
    for (auto& k : historyVector) {
        delete k;
        k = nullptr;
    }
    historyVector.clear();
}
//=============================================================================
void
XmlDocHistory::append(const std::wstring& version, const std::wstring& description)
{
    XmlDocHistoryItem* item = nullptr;
    try {
        item = new XmlDocHistoryItem(version, description);
    } catch (const std::bad_alloc&) {
        item = nullptr;
    }
    if (item) {
        historyVector.push_back(item);
    }
}
//=============================================================================
std::wstring
XmlDocHistory::getItemType()
{
    return utf8_to_wstring(HISTORY_TAG);
}
//=============================================================================
bool
XmlDocHistory::writeAsHtml(std::string& utf8stream)
{
    utf8stream = utf8stream + HTML_H3_IN_TAG + _("History") + HTML_H3_OUT_TAG + "\n";
    utf8stream = utf8stream + HTML_HR_OUT_TAG + "\n";
    utf8stream = utf8stream + "\n";
    utf8stream = utf8stream + R"(<table summary = "history" style="width:50%">)" + "\n";
    utf8stream = utf8stream + HTML_TR_IN_TAG + "\n";
    utf8stream = utf8stream + "\t" + HTML_TH_IN_TAG + _("Version") + HTML_TH_OUT_TAG + "\n";
    utf8stream = utf8stream + "\t" + HTML_TH_IN_TAG + _("Description") + HTML_TH_OUT_TAG + "\n";
    utf8stream = utf8stream + HTML_TR_OUT_TAG + "\n";
    for (auto& k : historyVector) {
        k->writeAsHtml(utf8stream);
    }
    utf8stream = utf8stream + HTML_TABLE_OUT_TAG + "\n";
    utf8stream = utf8stream + "\n";
    return true;
}
//=============================================================================
bool
XmlDocHistory::writeAsMarkdown(std::string& utf8stream)
{
    utf8stream = utf8stream + "## " + _("History") + "\n";
    utf8stream = utf8stream + "\n";
    utf8stream = utf8stream + "|" + _("Version") + "|" + _("Description") + "|" + "\n";
    utf8stream = utf8stream + "|" + "------" + "|" + "------" + "|" + "\n";
    for (auto& k : historyVector) {
        k->writeAsMarkdown(utf8stream);
    }
    utf8stream = utf8stream + "\n";
    return true;
}
//=============================================================================
}
//=============================================================================
