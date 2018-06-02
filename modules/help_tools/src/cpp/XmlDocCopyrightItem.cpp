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
#include "XmlDocCopyrightItem.hpp"
#include "HtmlTags.hpp"
#include "XmlDocumentTags.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
XmlDocCopyrightItem::XmlDocCopyrightItem(std::wstring copyright) { this->_copyright = copyright; }
//=============================================================================
XmlDocCopyrightItem::~XmlDocCopyrightItem() { this->_copyright = L""; }
//=============================================================================
void
XmlDocCopyrightItem::setValue(std::wstring value)
{
    this->_copyright = value;
}
//=============================================================================
std::wstring
XmlDocCopyrightItem::getValue()
{
    return this->_copyright;
}
//=============================================================================
std::wstring
XmlDocCopyrightItem::getItemType()
{
    return utf8_to_wstring(COPYRIGHT_TAG);
}
//=============================================================================
bool
XmlDocCopyrightItem::writeAsHtml(std::string& utf8stream)
{
    utf8stream = utf8stream + HTML_COMMENT_IN_TAG + "\n";
    utf8stream = utf8stream + wstring_to_utf8(this->_copyright) + "\n";
    utf8stream = utf8stream + HTML_COMMENT_OUT_TAG + "\n";
    utf8stream = utf8stream + "\n";
    return true;
}
//=============================================================================
bool
XmlDocCopyrightItem::writeAsMarkdown(std::string& utf8stream)
{
    return true;
}
//=============================================================================
}
//=============================================================================
