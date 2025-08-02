//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "XmlDocHistoryItem.hpp"
#include "HtmlTags.hpp"
#include "XmlDocumentTags.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
XmlDocHistoryItem::XmlDocHistoryItem(const std::wstring& version, const std::wstring& description)
{
    this->_version.assign(version);
    this->_description.assign(description);
}
//=============================================================================
XmlDocHistoryItem::~XmlDocHistoryItem()
{
    this->_version.clear();
    this->_description.clear();
}
//=============================================================================
std::wstring
XmlDocHistoryItem::getVersion()
{
    return this->_version;
}
//=============================================================================
std::wstring
XmlDocHistoryItem::getDescription()
{
    return this->_description;
}
//=============================================================================
std::wstring
XmlDocHistoryItem::getItemType()
{
    return utf8_to_wstring(HISTORY_TAG);
}
//=============================================================================
bool
XmlDocHistoryItem::writeAsHtml(std::string& utf8stream)
{
    utf8stream = utf8stream + HTML_TR_IN_TAG + "\n";
    utf8stream = utf8stream + "\t" + HTML_TD_IN_TAG + wstring_to_utf8(this->_version)
        + HTML_TD_OUT_TAG + "\n";
    utf8stream = utf8stream + "\t" + HTML_TD_IN_TAG + wstring_to_utf8(this->_description)
        + HTML_TD_OUT_TAG + "\n";
    utf8stream = utf8stream + HTML_TR_OUT_TAG + "\n";
    utf8stream = utf8stream + "\n";
    return true;
}
//=============================================================================
bool
XmlDocHistoryItem::writeAsMarkdown(std::string& utf8stream)
{
    utf8stream = utf8stream + "|" + wstring_to_utf8(this->_version);
    utf8stream = utf8stream + "|" + wstring_to_utf8(this->_description) + "|" + "\n";
    return true;
}
//=============================================================================
}
//=============================================================================
