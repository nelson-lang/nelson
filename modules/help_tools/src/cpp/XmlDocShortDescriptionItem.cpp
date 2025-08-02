//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "XmlDocShortDescriptionItem.hpp"
#include "HtmlTags.hpp"
#include "XmlDocumentTags.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
XmlDocShortDescriptionItem::XmlDocShortDescriptionItem(const std::wstring& description)
{
    this->_description.assign(description);
}
//=============================================================================
XmlDocShortDescriptionItem::~XmlDocShortDescriptionItem() { this->_description.clear(); }
//=============================================================================
void
XmlDocShortDescriptionItem::setValue(const std::wstring& value)
{
    this->_description.assign(value);
}
//=============================================================================
std::wstring
XmlDocShortDescriptionItem::getValue()
{
    return this->_description;
}
//=============================================================================
std::wstring
XmlDocShortDescriptionItem::getItemType()
{
    return utf8_to_wstring(SHORT_DESCRIPTION_TAG);
}
//=============================================================================
bool
XmlDocShortDescriptionItem::writeAsHtml(std::string& utf8stream)
{
    utf8stream
        = utf8stream + HTML_H3_IN_TAG + wstring_to_utf8(_description) + HTML_H3_OUT_TAG + "\n";
    utf8stream = utf8stream + HTML_HR_OUT_TAG + "\n";
    utf8stream = utf8stream + "\n";
    return true;
}
//=============================================================================
bool
XmlDocShortDescriptionItem::writeAsMarkdown(std::string& utf8stream)
{
    utf8stream = utf8stream + wstring_to_utf8(_description) + "\n";
    utf8stream = utf8stream + "\n";
    return true;
}
//=============================================================================
}
//=============================================================================
