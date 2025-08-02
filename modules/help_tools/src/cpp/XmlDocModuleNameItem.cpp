//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
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
