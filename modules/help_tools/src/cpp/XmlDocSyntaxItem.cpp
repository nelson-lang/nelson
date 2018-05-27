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
#include "XmlDocSyntaxItem.hpp"
#include "HtmlTags.hpp"
#include "XmlDocumentTags.hpp"
#include "characters_encoding.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
XmlDocSyntaxItem::XmlDocSyntaxItem(std::wstring syntax) { this->_syntax = syntax; }
//=============================================================================
XmlDocSyntaxItem::~XmlDocSyntaxItem() { this->_syntax = L""; }
//=============================================================================
std::wstring
XmlDocSyntaxItem::getValue()
{
    return this->_syntax;
}
//=============================================================================
std::wstring
XmlDocSyntaxItem::getItemType()
{
    return utf8_to_wstring(SYNTAX_TAG);
}
//=============================================================================
bool
XmlDocSyntaxItem::writeAsHtml(std::string& utf8stream)
{
    utf8stream = utf8stream + "\t" + HTML_TR_IN_TAG + "\n";
    utf8stream = utf8stream + "\t\t" + HTML_TD_IN_TAG + wstring_to_utf8(this->getValue())
        + HTML_TD_OUT_TAG + "\n";
    utf8stream = utf8stream + "\t" + HTML_TR_OUT_TAG + "\n";
    utf8stream = utf8stream + "\n";
    return true;
}
//=============================================================================
bool
XmlDocSyntaxItem::writeAsMarkdown(std::string& utf8stream)
{
    utf8stream = utf8stream + "- " + wstring_to_utf8(this->getValue()) + "\n";
    return true;
}
//=============================================================================
}
//=============================================================================
