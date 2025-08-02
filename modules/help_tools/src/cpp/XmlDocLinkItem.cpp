//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "XmlDocLinkItem.hpp"
#include "HtmlTags.hpp"
#include "XmlDocumentTags.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
XmlDocLinkItem::XmlDocLinkItem(const std::wstring& name, const std::wstring& link)
{
    this->_name.assign(name);
    this->_link.assign(link);
}
//=============================================================================
XmlDocLinkItem::~XmlDocLinkItem()
{
    this->_name.clear();
    this->_link.clear();
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
    utf8stream = utf8stream + "<a href = \"" + wstring_to_utf8(this->_link) + R"(" class = "link">)"
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
