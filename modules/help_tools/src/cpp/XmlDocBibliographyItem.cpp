//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "XmlDocBibliographyItem.hpp"

#include <utility>
#include "HtmlTags.hpp"
#include "XmlDocumentTags.hpp"
#include "characters_encoding.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
XmlDocBibliographyItem::XmlDocBibliographyItem(std::wstring bibliography)
    : _bibliography(std::move(bibliography))
{
}
//=============================================================================
XmlDocBibliographyItem::~XmlDocBibliographyItem() { this->_bibliography = L""; }
//=============================================================================
std::wstring
XmlDocBibliographyItem::getItemType()
{
    return utf8_to_wstring(BIBLIOGRAPHY_TAG);
}
//=============================================================================
bool
XmlDocBibliographyItem::writeAsHtml(std::string& utf8stream)
{
    if (!this->_bibliography.empty()) {
        utf8stream = utf8stream + HTML_H3_IN_TAG + _("Bibliography") + HTML_H3_OUT_TAG + "\n";
        utf8stream = utf8stream + HTML_HR_OUT_TAG + "\n";
        utf8stream = utf8stream + wstring_to_utf8(this->_bibliography) + "\n";
        utf8stream = utf8stream + "\n";
    }
    return true;
}
//=============================================================================
bool
XmlDocBibliographyItem::writeAsMarkdown(std::string& utf8stream)
{
    if (!this->_bibliography.empty()) {
        utf8stream = utf8stream + _("Bibliography") + "\n";
        utf8stream = utf8stream + "\n";
        utf8stream = utf8stream + wstring_to_utf8(this->_bibliography) + "\n";
        utf8stream = utf8stream + "\n";
    }
    return true;
}
//=============================================================================
}
//=============================================================================
