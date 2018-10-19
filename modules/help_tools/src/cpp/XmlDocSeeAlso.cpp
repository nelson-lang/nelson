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
#include "XmlDocSeeAlso.hpp"
#include "HtmlTags.hpp"
#include "XmlDocumentTags.hpp"
#include "characters_encoding.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
XmlDocSeeAlso::XmlDocSeeAlso() { seeAlsoVector.clear(); }
//=============================================================================
XmlDocSeeAlso::~XmlDocSeeAlso()
{
    for (size_t k = 0; k < seeAlsoVector.size(); k++) {
        delete seeAlsoVector[k];
        seeAlsoVector[k] = nullptr;
    }
    seeAlsoVector.clear();
}
//=============================================================================
void
XmlDocSeeAlso::append(std::wstring name, std::wstring link)
{
    XmlDocSeeAlsoItem* item = nullptr;
    try {
        item = new XmlDocSeeAlsoItem(name, link);
    } catch (const std::bad_alloc&) {
        item = nullptr;
    }
    if (item) {
        seeAlsoVector.push_back(item);
    }
}
//=============================================================================
std::wstring
XmlDocSeeAlso::getItemType()
{
    return utf8_to_wstring(SEE_ALSO_TAG);
}
//=============================================================================
bool
XmlDocSeeAlso::writeAsHtml(std::string& utf8stream)
{
    utf8stream = utf8stream + HTML_H3_IN_TAG + _("See also") + HTML_H3_OUT_TAG + "\n";
    utf8stream = utf8stream + HTML_HR_OUT_TAG + "\n";
    utf8stream = utf8stream + "\n";
    utf8stream = utf8stream + HTML_P_IN_TAG + "\n";
    for (size_t k = 0; k < seeAlsoVector.size(); k++) {
        seeAlsoVector[k]->writeAsHtml(utf8stream);
        if (k < seeAlsoVector.size() - 1) {
            utf8stream = utf8stream + ", ";
        } else {
            utf8stream = utf8stream + ".";
        }
    }
    utf8stream = utf8stream + HTML_P_OUT_TAG + "\n";
    utf8stream = utf8stream + "\n";
    return true;
}
//=============================================================================
bool
XmlDocSeeAlso::writeAsMarkdown(std::string& utf8stream)
{
    utf8stream = utf8stream + "## " + _("See also") + "\n";
    utf8stream = utf8stream + "\n";
    for (size_t k = 0; k < seeAlsoVector.size(); k++) {
        seeAlsoVector[k]->writeAsMarkdown(utf8stream);
        if (k < seeAlsoVector.size() - 1) {
            utf8stream = utf8stream + ", ";
        } else {
            utf8stream = utf8stream + ".";
        }
    }
    utf8stream = utf8stream + "\n";
    return true;
}
//=============================================================================
}
//=============================================================================
