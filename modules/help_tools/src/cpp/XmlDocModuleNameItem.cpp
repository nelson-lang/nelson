//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// Alternatively, you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of
// the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this program. If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#include "XmlDocModuleNameItem.hpp"
#include "HtmlTags.hpp"
#include "XmlDocumentTags.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
XmlDocModuleNameItem::XmlDocModuleNameItem(const std::wstring& module_name)
{
    this->_module_name.assign(module_name);
}
//=============================================================================
XmlDocModuleNameItem::~XmlDocModuleNameItem() { this->_module_name.clear(); }
//=============================================================================
void
XmlDocModuleNameItem::setValue(const std::wstring& value)
{
    this->_module_name.assign(value);
}
//=============================================================================
std::wstring
XmlDocModuleNameItem::getValue()
{
    return this->_module_name;
}
//=============================================================================
std::wstring
XmlDocModuleNameItem::getItemType()
{
    return utf8_to_wstring(MODULE_NAME_TAG);
}
//=============================================================================
bool
XmlDocModuleNameItem::writeAsHtml(std::string& utf8stream)
{
    return true;
}
//=============================================================================
bool
XmlDocModuleNameItem::writeAsMarkdown(std::string& utf8stream)
{
    return true;
}
//=============================================================================
}
//=============================================================================
