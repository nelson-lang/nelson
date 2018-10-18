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
    for (size_t k = 0; k < historyVector.size(); k++) {
        delete historyVector[k];
        historyVector[k] = nullptr;
    }
    historyVector.clear();
}
//=============================================================================
void
XmlDocHistory::append(std::wstring version, std::wstring description)
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
    utf8stream = utf8stream + "<table summary = \"history\" style=\"width:50%\">" + "\n";
    utf8stream = utf8stream + HTML_TR_IN_TAG + "\n";
    utf8stream = utf8stream + "\t" + HTML_TH_IN_TAG + _("Version") + HTML_TH_OUT_TAG + "\n";
    utf8stream = utf8stream + "\t" + HTML_TH_IN_TAG + _("Description") + HTML_TH_OUT_TAG + "\n";
    utf8stream = utf8stream + HTML_TR_OUT_TAG + "\n";
    for (size_t k = 0; k < historyVector.size(); k++) {
        historyVector[k]->writeAsHtml(utf8stream);
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
    for (size_t k = 0; k < historyVector.size(); k++) {
        historyVector[k]->writeAsMarkdown(utf8stream);
    }
    utf8stream = utf8stream + "\n";
    return true;
}
//=============================================================================
}
//=============================================================================
