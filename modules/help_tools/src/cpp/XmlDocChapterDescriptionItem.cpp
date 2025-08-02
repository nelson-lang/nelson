//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "XmlDocChapterDescriptionItem.hpp"

#include <utility>
#include "HtmlTags.hpp"
#include "XmlDocumentTags.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
XmlDocChapterDescriptionItem::XmlDocChapterDescriptionItem(std::wstring description)
    : _description(std::move(description))
{
}
//=============================================================================
XmlDocChapterDescriptionItem::~XmlDocChapterDescriptionItem() { this->_description.clear(); }
//=============================================================================
void
XmlDocChapterDescriptionItem::setValue(const std::wstring& value)
{
    this->_description = value;
}
//=============================================================================
std::wstring
XmlDocChapterDescriptionItem::getValue()
{
    return this->_description;
}
//=============================================================================
std::wstring
XmlDocChapterDescriptionItem::getItemType()
{
    return utf8_to_wstring(CHAPTER_DESCRIPTION_TAG);
}
//=============================================================================
bool
XmlDocChapterDescriptionItem::writeAsHtml(std::string& utf8stream)
{
    utf8stream
        = utf8stream + HTML_H3_IN_TAG + wstring_to_utf8(_description) + HTML_H3_OUT_TAG + "\n";
    utf8stream = utf8stream + HTML_HR_OUT_TAG + "\n";
    utf8stream = utf8stream + "\n";
    return true;
}
//=============================================================================
bool
XmlDocChapterDescriptionItem::writeAsMarkdown(std::string& utf8stream)
{
    utf8stream = utf8stream + "## Description" + "\n";
    utf8stream = utf8stream + wstring_to_utf8(_description) + "\n";
    utf8stream = utf8stream + "\n";
    return true;
}
//=============================================================================
}
//=============================================================================
