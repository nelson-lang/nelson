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
#include "XmlDocSyntax.hpp"
#include "HtmlTags.hpp"
#include "XmlDocumentTags.hpp"
#include "characters_encoding.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
XmlDocSyntax::XmlDocSyntax() { syntaxVector.clear(); }
//=============================================================================
XmlDocSyntax::~XmlDocSyntax()
{
    for (size_t k = 0; k < syntaxVector.size(); k++) {
        delete syntaxVector[k];
        syntaxVector[k] = nullptr;
    }
    syntaxVector.clear();
}
//=============================================================================
void
XmlDocSyntax::append(std::wstring value)
{
    XmlDocSyntaxItem* item = nullptr;
    try {
        item = new XmlDocSyntaxItem(value);
    } catch (const std::bad_alloc&) {
        item = nullptr;
    }
    if (item) {
        syntaxVector.push_back(item);
    }
}
//=============================================================================
std::wstring
XmlDocSyntax::getItemType()
{
    return utf8_to_wstring(SYNTAX_TAG);
}
//=============================================================================
bool
XmlDocSyntax::writeAsHtml(std::string& utf8stream)
{
    utf8stream = utf8stream + HTML_H3_IN_TAG + _("Syntax") + HTML_H3_OUT_TAG + "\n";
    utf8stream = utf8stream + HTML_HR_OUT_TAG + "\n";
    utf8stream = utf8stream + "\n";
    utf8stream = utf8stream + "<table summary=\"syntax\" style=\"width:50%\">" + "\n";
    for (size_t k = 0; k < syntaxVector.size(); k++) {
        syntaxVector[k]->writeAsHtml(utf8stream);
    }
    utf8stream = utf8stream + HTML_TABLE_OUT_TAG + "\n";
    utf8stream = utf8stream + "\n";
    return true;
}
//=============================================================================
bool
XmlDocSyntax::writeAsMarkdown(std::string& utf8stream)
{
    utf8stream = utf8stream + "## " + _("Syntax") + "\n";
    utf8stream = utf8stream + "\n";
    for (size_t k = 0; k < syntaxVector.size(); k++) {
        syntaxVector[k]->writeAsMarkdown(utf8stream);
    }
    utf8stream = utf8stream + "\n";
    return true;
}
//=============================================================================
}
//=============================================================================
