//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "XmlDocSeeAlso.hpp"
#include "HtmlTags.hpp"
#include "XmlDocumentTags.hpp"
#include "characters_encoding.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
XmlDocSeeAlso::XmlDocSeeAlso() { seeAlsoVector.clear(); }
//=============================================================================
XmlDocSeeAlso::~XmlDocSeeAlso()
{
    for (auto& k : seeAlsoVector) {
        delete k;
        k = nullptr;
    }
    seeAlsoVector.clear();
}
//=============================================================================
void
XmlDocSeeAlso::append(const std::wstring& name, const std::wstring& link)
{
    XmlDocSeeAlsoItem* item = nullptr;
    try {
        item = new XmlDocSeeAlsoItem(name, link);
    } catch (const std::bad_alloc&) {
        item = nullptr;
    }
    if (item) {
        seeAlsoVector.push_back(item);
    }
}
//=============================================================================
std::wstring
XmlDocSeeAlso::getItemType()
{
    return utf8_to_wstring(SEE_ALSO_TAG);
}
//=============================================================================
bool
XmlDocSeeAlso::writeAsHtml(std::string& utf8stream)
{
    utf8stream = utf8stream + HTML_H3_IN_TAG + _("See also") + HTML_H3_OUT_TAG + "\n";
    utf8stream = utf8stream + HTML_HR_OUT_TAG + "\n";
    utf8stream = utf8stream + "\n";
    utf8stream = utf8stream + HTML_P_IN_TAG + "\n";
    for (size_t k = 0; k < seeAlsoVector.size(); k++) {
        seeAlsoVector[k]->writeAsHtml(utf8stream);
        if (k < seeAlsoVector.size() - 1) {
            utf8stream = utf8stream + ", ";
        } else {
            utf8stream = utf8stream + ".";
        }
    }
    utf8stream = utf8stream + HTML_P_OUT_TAG + "\n";
    utf8stream = utf8stream + "\n";
    return true;
}
//=============================================================================
bool
XmlDocSeeAlso::writeAsMarkdown(std::string& utf8stream)
{
    utf8stream = utf8stream + "## " + _("See also") + "\n";
    utf8stream = utf8stream + "\n";
    for (size_t k = 0; k < seeAlsoVector.size(); k++) {
        seeAlsoVector[k]->writeAsMarkdown(utf8stream);
        if (k < seeAlsoVector.size() - 1) {
            utf8stream = utf8stream + ", ";
        } else {
            utf8stream = utf8stream + ".";
        }
    }
    utf8stream = utf8stream + "\n";
    return true;
}
//=============================================================================
}
//=============================================================================
