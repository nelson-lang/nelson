//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// Alternatively, you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of
// the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this program. If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#include "XmlDocChapterItem.hpp"

#include <utility>
#include "HtmlTags.hpp"
#include "XmlDocumentTags.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
XmlDocChapterItem::XmlDocChapterItem(std::wstring chapter) : _chapter(std::move(chapter)) {}
//=============================================================================
XmlDocChapterItem::~XmlDocChapterItem() { this->_chapter.clear(); }
//=============================================================================
void
XmlDocChapterItem::setValue(const std::wstring& value)
{
    this->_chapter = value;
}
//=============================================================================
std::wstring
XmlDocChapterItem::getValue()
{
    return this->_chapter;
}
//=============================================================================
std::wstring
XmlDocChapterItem::getItemType()
{
    return utf8_to_wstring(CHAPTER_TAG);
}
//=============================================================================
bool
XmlDocChapterItem::writeHeaderAsHtml(std::string& utf8stream)
{
    utf8stream = utf8stream + "\t"
        + R"(<meta http-equiv="Content-Type" content="text/html; charset=utf-8" />)" + "\n";
    utf8stream = utf8stream + "\t" + HTML_TITLE_IN_TAG + wstring_to_utf8(_chapter)
        + HTML_TITLE_OUT_TAG + "\n";
    return true;
}
//=============================================================================
bool
XmlDocChapterItem::writeAsHtml(std::string& utf8stream)
{
    utf8stream = utf8stream + HTML_H3_IN_TAG + wstring_to_utf8(_chapter) + HTML_H3_OUT_TAG + "\n";
    utf8stream = utf8stream + HTML_HR_OUT_TAG + "\n";
    utf8stream = utf8stream + "\n";
    return true;
}
//=============================================================================
bool
XmlDocChapterItem::writeHeaderAsMarkdown(std::string& utf8stream)
{
    utf8stream = utf8stream + "\n";
    utf8stream = utf8stream + "# " + wstring_to_utf8(_chapter) + "\n";
    return true;
}
//=============================================================================
bool
XmlDocChapterItem::writeAsMarkdown(std::string& utf8stream)
{
    utf8stream = utf8stream + wstring_to_utf8(_chapter) + "\n";
    utf8stream = utf8stream + "\n";
    return true;
}
//=============================================================================
}
//=============================================================================
