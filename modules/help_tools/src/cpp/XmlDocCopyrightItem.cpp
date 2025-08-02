//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "XmlDocCopyrightItem.hpp"

#include <utility>
#include "HtmlTags.hpp"
#include "XmlDocumentTags.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
XmlDocCopyrightItem::XmlDocCopyrightItem(std::wstring copyright) : _copyright(std::move(copyright))
{
}
//=============================================================================
XmlDocCopyrightItem::~XmlDocCopyrightItem() { this->_copyright.clear(); }
//=============================================================================
void
XmlDocCopyrightItem::setValue(const std::wstring& value)
{
    this->_copyright = value;
}
//=============================================================================
std::wstring
XmlDocCopyrightItem::getValue()
{
    return this->_copyright;
}
//=============================================================================
std::wstring
XmlDocCopyrightItem::getItemType()
{
    return utf8_to_wstring(COPYRIGHT_TAG);
}
//=============================================================================
bool
XmlDocCopyrightItem::writeAsHtml(std::string& utf8stream)
{
    utf8stream = utf8stream + HTML_COMMENT_IN_TAG + "\n";
    utf8stream = utf8stream + wstring_to_utf8(this->_copyright) + "\n";
    utf8stream = utf8stream + HTML_COMMENT_OUT_TAG + "\n";
    utf8stream = utf8stream + "\n";
    return true;
}
//=============================================================================
bool
XmlDocCopyrightItem::writeAsMarkdown(std::string& utf8stream)
{
    return true;
}
//=============================================================================
}
//=============================================================================
