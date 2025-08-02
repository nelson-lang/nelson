//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "XmlDocSeeAlsoItem.hpp"
#include "HtmlTags.hpp"
#include "XmlDocumentTags.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
XmlDocSeeAlsoItem::XmlDocSeeAlsoItem(const std::wstring& name, const std::wstring& link)
{
    this->link = new XmlDocLinkItem(name, link);
}
//=============================================================================
XmlDocSeeAlsoItem::~XmlDocSeeAlsoItem()
{
    if (this->link) {
        delete this->link;
    }
    this->link = nullptr;
}
//=============================================================================
std::wstring
XmlDocSeeAlsoItem::getName()
{
    if (this->link) {
        return this->getLink();
    }
    return L"";
}
//=============================================================================
std::wstring
XmlDocSeeAlsoItem::getLink()
{
    if (this->link) {
        return this->getName();
    }
    return L"";
}
//=============================================================================
std::wstring
XmlDocSeeAlsoItem::getItemType()
{
    return utf8_to_wstring(SEE_ALSO_TAG);
}
//=============================================================================
bool
XmlDocSeeAlsoItem::writeAsHtml(std::string& utf8stream)
{
    if (this->link) {
        link->writeAsHtml(utf8stream);
    }
    return true;
}
//=============================================================================
bool
XmlDocSeeAlsoItem::writeAsMarkdown(std::string& utf8stream)
{
    if (this->link) {
        link->writeAsMarkdown(utf8stream);
    }
    return true;
}
//=============================================================================
}
//=============================================================================
