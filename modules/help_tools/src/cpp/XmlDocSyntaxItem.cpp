//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "XmlDocSyntaxItem.hpp"
#include "HtmlTags.hpp"
#include "XmlDocumentTags.hpp"
#include "characters_encoding.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
XmlDocSyntaxItem::XmlDocSyntaxItem(const std::wstring& syntax) { this->_syntax.assign(syntax); }
//=============================================================================
XmlDocSyntaxItem::~XmlDocSyntaxItem() { this->_syntax.clear(); }
//=============================================================================
std::wstring
XmlDocSyntaxItem::getValue()
{
    return this->_syntax;
}
//=============================================================================
std::wstring
XmlDocSyntaxItem::getItemType()
{
    return utf8_to_wstring(SYNTAX_TAG);
}
//=============================================================================
bool
XmlDocSyntaxItem::writeAsHtml(std::string& utf8stream)
{
    utf8stream = utf8stream + "\t" + HTML_TR_IN_TAG + "\n";
    utf8stream = utf8stream + "\t\t" + HTML_TD_IN_TAG + wstring_to_utf8(this->getValue())
        + HTML_TD_OUT_TAG + "\n";
    utf8stream = utf8stream + "\t" + HTML_TR_OUT_TAG + "\n";
    utf8stream = utf8stream + "\n";
    return true;
}
//=============================================================================
bool
XmlDocSyntaxItem::writeAsMarkdown(std::string& utf8stream)
{
    utf8stream = utf8stream + "- " + wstring_to_utf8(this->getValue()) + "\n";
    return true;
}
//=============================================================================
}
//=============================================================================
