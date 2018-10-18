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
    for (size_t k = 0; k < paramOutputItemVector.size(); k++) {
        delete paramOutputItemVector[k];
        paramOutputItemVector[k] = nullptr;
    }
    paramOutputItemVector.clear();
}
//=============================================================================
void
XmlDocParamOutput::append(std::wstring name, std::wstring description)
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
    for (size_t k = 0; k < paramOutputItemVector.size(); k++) {
        paramOutputItemVector[k]->writeAsHtml(utf8stream);
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
    for (size_t k = 0; k < paramOutputItemVector.size(); k++) {
        paramOutputItemVector[k]->writeAsMarkdown(utf8stream);
    }
    utf8stream = utf8stream + "\n";
    return true;
}
//=============================================================================
}
//=============================================================================
