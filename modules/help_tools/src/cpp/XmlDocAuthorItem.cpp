//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "XmlDocAuthorItem.hpp"

#include <utility>
#include "HtmlTags.hpp"
#include "XmlDocumentTags.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
XmlDocAuthorItem::XmlDocAuthorItem(std::wstring author) : _author(std::move(author)) {};
//=============================================================================
XmlDocAuthorItem::~XmlDocAuthorItem() { this->_author = L""; }
//=============================================================================
std::wstring
XmlDocAuthorItem::getItemType()
{
    return utf8_to_wstring(AUTHOR_ITEM_TAG);
}
//=============================================================================
bool
XmlDocAuthorItem::writeAsHtml(std::string& utf8stream)
{
    utf8stream
        = utf8stream + HTML_P_IN_TAG + wstring_to_utf8(this->_author) + HTML_P_OUT_TAG + "\n";
    utf8stream = utf8stream + "\n";
    return true;
}
//=============================================================================
bool
XmlDocAuthorItem::writeAsMarkdown(std::string& utf8stream)
{
    utf8stream = utf8stream + wstring_to_utf8(this->_author) + "\n";
    utf8stream = utf8stream + "\n";
    return true;
}
//=============================================================================
}
//=============================================================================
