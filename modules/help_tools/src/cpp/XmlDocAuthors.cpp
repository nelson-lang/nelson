//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "XmlDocAuthors.hpp"
#include "HtmlTags.hpp"
#include "XmlDocumentTags.hpp"
#include "characters_encoding.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
XmlDocAuthors::XmlDocAuthors() { authorVector.clear(); }
//=============================================================================
XmlDocAuthors::~XmlDocAuthors()
{
    for (auto& k : authorVector) {
        delete k;
        k = nullptr;
    }
    authorVector.clear();
}
//=============================================================================
void
XmlDocAuthors::append(const std::wstring& value)
{
    XmlDocAuthorItem* item = nullptr;
    try {
        item = new XmlDocAuthorItem(value);
    } catch (const std::bad_alloc&) {
        item = nullptr;
    }
    if (item) {
        authorVector.push_back(item);
    }
}
//=============================================================================
std::wstring
XmlDocAuthors::getItemType()
{
    return utf8_to_wstring(AUTHORS_TAG);
}
//=============================================================================
bool
XmlDocAuthors::writeAsHtml(std::string& utf8stream)
{
    if (authorVector.size() > 1) {
        utf8stream = utf8stream + HTML_H3_IN_TAG + _("Authors") + HTML_H3_OUT_TAG + "\n";
    } else {
        utf8stream = utf8stream + HTML_H3_IN_TAG + _("Author") + HTML_H3_OUT_TAG + "\n";
    }
    utf8stream = utf8stream + HTML_HR_OUT_TAG + "\n";
    utf8stream = utf8stream + "\n";
    for (auto& k : authorVector) {
        k->writeAsHtml(utf8stream);
    }
    return true;
}
//=============================================================================
bool
XmlDocAuthors::writeAsMarkdown(std::string& utf8stream)
{
    if (authorVector.size() > 1) {
        utf8stream = utf8stream + "## " + _("Authors") + "\n";
    } else {
        utf8stream = utf8stream + "## " + _("Author") + "\n";
    }
    utf8stream = utf8stream + "\n";
    for (auto& k : authorVector) {
        k->writeAsMarkdown(utf8stream);
    }
    return true;
}
//=============================================================================
}
//=============================================================================
