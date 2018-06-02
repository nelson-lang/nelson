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
#include "XmlDocLanguageItem.hpp"
#include "XmlDocumentTags.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
XmlDocLanguageItem::XmlDocLanguageItem(std::wstring language) { this->_language = language; }
//=============================================================================
XmlDocLanguageItem::~XmlDocLanguageItem() { this->_language = L""; }
//=============================================================================
void
XmlDocLanguageItem::setValue(std::wstring value)
{
    this->_language = value;
}
//=============================================================================
std::wstring
XmlDocLanguageItem::getValue()
{
    return this->_language;
}
//=============================================================================
std::wstring
XmlDocLanguageItem::getItemType()
{
    return utf8_to_wstring(LANGUAGE_TAG);
}
//=============================================================================
bool
XmlDocLanguageItem::writeAsHtml(std::string& utf8stream)
{
    return true;
}
//=============================================================================
bool
XmlDocLanguageItem::writeAsMarkdown(std::string& utf8stream)
{
    return true;
}
//=============================================================================
}
//=============================================================================
