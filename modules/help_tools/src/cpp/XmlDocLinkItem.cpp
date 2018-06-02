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
#include "XmlDocLinkItem.hpp"
#include "HtmlTags.hpp"
#include "XmlDocumentTags.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
XmlDocLinkItem::XmlDocLinkItem(std::wstring name, std::wstring link)
{
    this->_name = name;
    this->_link = link;
}
//=============================================================================
XmlDocLinkItem::~XmlDocLinkItem()
{
    this->_name = L"";
    this->_link = L"";
}
//=============================================================================
std::wstring
XmlDocLinkItem::getName()
{
    return this->_name;
}
//=============================================================================
std::wstring
XmlDocLinkItem::getLink()
{
    return this->_link;
}
//=============================================================================
std::wstring
XmlDocLinkItem::getItemType()
{
    return utf8_to_wstring(XML_LINK_TAG);
}
//=============================================================================
bool
XmlDocLinkItem::writeAsHtml(std::string& utf8stream)
{
    utf8stream = utf8stream + "<a href = \"" + wstring_to_utf8(this->_link) + "\" class = \"link\">"
        + wstring_to_utf8(this->_name) + HTML_A_OUT_TAG;
    return true;
}
//=============================================================================
bool
XmlDocLinkItem::writeAsMarkdown(std::string& utf8stream)
{
    utf8stream = utf8stream + "[" + wstring_to_utf8(this->_name) + "]("
        + wstring_to_utf8(this->_link) + ")";
    return true;
}
//=============================================================================
}
//=============================================================================
