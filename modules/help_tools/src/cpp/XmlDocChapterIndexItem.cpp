//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "XmlDocChapterIndexItem.hpp"
#include "XmlDocumentTags.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
XmlDocChapterIndexItem::XmlDocChapterIndexItem() { chapterRefVector.clear(); }
//=============================================================================
XmlDocChapterIndexItem::~XmlDocChapterIndexItem()
{
    for (auto& k : chapterRefVector) {
        delete k;
        k = nullptr;
    }
    chapterRefVector.clear();
}
//=============================================================================
std::wstring
XmlDocChapterIndexItem::getItemType()
{
    return utf8_to_wstring(CHAPTER_INDEX_TAG);
}
//=============================================================================
bool
XmlDocChapterIndexItem::append(
    const std::wstring& linkname, const std::wstring& linkurl, const std::wstring& description)
{
    XmlDocChapterRefItem* item = nullptr;
    try {
        item = new XmlDocChapterRefItem(linkname, linkurl, description);
    } catch (const std::bad_alloc&) {
        item = nullptr;
    }
    if (item) {
        chapterRefVector.push_back(item);
    }
    return true;
}
//=============================================================================
bool
XmlDocChapterIndexItem::writeAsHtml(std::string& utf8stream)
{
    utf8stream = utf8stream + "<ul class=\"list-chapter\">" + "\n";
    for (auto& k : chapterRefVector) {
        k->writeAsHtml(utf8stream);
    }
    utf8stream = utf8stream + "</ul>" + "\n";
    utf8stream = utf8stream + "\n";
    return true;
}
//=============================================================================
bool
XmlDocChapterIndexItem::writeAsMarkdown(std::string& utf8stream)
{
    utf8stream = utf8stream + "\n";
    for (auto& k : chapterRefVector) {
        k->writeAsMarkdown(utf8stream);
    }
    utf8stream = utf8stream + "\n";
    return true;
}
//=============================================================================
}
//=============================================================================
