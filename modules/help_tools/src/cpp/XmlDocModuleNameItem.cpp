//=============================================================================
// Copyright (c) 2016-2017 Allan CORNET (Nelson)
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
#include "XmlDocModuleNameItem.hpp"
#include "characters_encoding.hpp"
#include "XmlDocumentTags.hpp"
#include "HtmlTags.hpp"
//=============================================================================
namespace Nelson {
    //=============================================================================
    XmlDocModuleNameItem::XmlDocModuleNameItem(std::wstring module_name)
    {
        this->_module_name = module_name;
    }
    //=============================================================================
    XmlDocModuleNameItem::~XmlDocModuleNameItem()
    {
        this->_module_name = L"";
    }
    //=============================================================================
    void XmlDocModuleNameItem::setValue(std::wstring value)
    {
        this->_module_name = value;
    }
    //=============================================================================
    std::wstring XmlDocModuleNameItem::getValue()
    {
        return this->_module_name;
    }
    //=============================================================================
    std::wstring XmlDocModuleNameItem::getItemType()
    {
        return utf8_to_wstring(MODULE_NAME_TAG);
    }
    //=============================================================================
    bool XmlDocModuleNameItem::writeAsHtml(std::string &utf8stream)
    {
        return true;
    }
    //=============================================================================
}
//=============================================================================
