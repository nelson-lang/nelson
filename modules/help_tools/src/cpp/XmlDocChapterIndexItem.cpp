//=============================================================================
// Copyright (c) 2016-2018 Allan CORNET (Nelson)
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
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
    for (size_t k = 0; k < chapterRefVector.size(); k++) {
        delete chapterRefVector[k];
        chapterRefVector[k] = nullptr;
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
    std::wstring linkname, std::wstring linkurl, std::wstring description)
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
    for (size_t k = 0; k < chapterRefVector.size(); k++) {
        chapterRefVector[k]->writeAsHtml(utf8stream);
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
    for (size_t k = 0; k < chapterRefVector.size(); k++) {
        chapterRefVector[k]->writeAsMarkdown(utf8stream);
    }
    utf8stream = utf8stream + "\n";
    return true;
}
//=============================================================================
}
//=============================================================================
