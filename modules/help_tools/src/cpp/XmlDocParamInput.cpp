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
    for (size_t k = 0; k < paramInputItemVector.size(); k++) {
        delete paramInputItemVector[k];
        paramInputItemVector[k] = nullptr;
    }
    paramInputItemVector.clear();
}
//=============================================================================
void
XmlDocParamInput::append(std::wstring name, std::wstring description)
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
    for (size_t k = 0; k < paramInputItemVector.size(); k++) {
        paramInputItemVector[k]->writeAsHtml(utf8stream);
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
    for (size_t k = 0; k < paramInputItemVector.size(); k++) {
        paramInputItemVector[k]->writeAsMarkdown(utf8stream);
    }
    utf8stream = utf8stream + "\n";
    return true;
}
//=============================================================================
}
//=============================================================================
