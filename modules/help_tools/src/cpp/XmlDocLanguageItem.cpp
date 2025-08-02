//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "XmlDocLanguageItem.hpp"
#include "XmlDocumentTags.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
XmlDocLanguageItem::XmlDocLanguageItem(const std::wstring& language)
{
    this->_language.assign(language);
}
//=============================================================================
XmlDocLanguageItem::~XmlDocLanguageItem() { this->_language.clear(); }
//=============================================================================
void
XmlDocLanguageItem::setValue(const std::wstring& value)
{
    this->_language.assign(value);
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
