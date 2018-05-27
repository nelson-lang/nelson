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
#include "XmlDocSeeAlsoItem.hpp"
#include "HtmlTags.hpp"
#include "XmlDocumentTags.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
XmlDocSeeAlsoItem::XmlDocSeeAlsoItem(std::wstring name, std::wstring link)
{
    this->link = new XmlDocLinkItem(name, link);
}
//=============================================================================
XmlDocSeeAlsoItem::~XmlDocSeeAlsoItem()
{
    if (this->link) {
        delete this->link;
    }
    this->link = nullptr;
}
//=============================================================================
std::wstring
XmlDocSeeAlsoItem::getName()
{
    if (this->link) {
        return this->getLink();
    }
    return L"";
}
//=============================================================================
std::wstring
XmlDocSeeAlsoItem::getLink()
{
    if (this->link) {
        return this->getName();
    }
    return L"";
}
//=============================================================================
std::wstring
XmlDocSeeAlsoItem::getItemType()
{
    return utf8_to_wstring(SEE_ALSO_TAG);
}
//=============================================================================
bool
XmlDocSeeAlsoItem::writeAsHtml(std::string& utf8stream)
{
    if (this->link) {
        link->writeAsHtml(utf8stream);
    }
    return true;
}
//=============================================================================
bool
XmlDocSeeAlsoItem::writeAsMarkdown(std::string& utf8stream)
{
    if (this->link) {
        link->writeAsMarkdown(utf8stream);
    }
    return true;
}
//=============================================================================
}
//=============================================================================
