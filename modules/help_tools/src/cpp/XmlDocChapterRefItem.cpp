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
#include "XmlDocChapterRefItem.hpp"
#include "HtmlTags.hpp"
#include "XmlDocumentTags.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
XmlDocChapterRefItem::XmlDocChapterRefItem(
    const std::wstring& linkname, const std::wstring& linkurl, const std::wstring& description)
    : _description(description), _linkname(linkname), _linkurl(linkurl)
{}
//=============================================================================
XmlDocChapterRefItem::~XmlDocChapterRefItem()
{
    this->_description.clear();
    this->_linkname.clear();
    this->_linkurl.clear();
}
//=============================================================================
std::wstring
XmlDocChapterRefItem::getItemType()
{
    return utf8_to_wstring(CHAPTER_REF_TAG);
}
//=============================================================================
bool
XmlDocChapterRefItem::writeAsHtml(std::string& utf8stream)
{
    if (!this->_description.empty()) {
        utf8stream = utf8stream + HTML_LI_IN_TAG + "<a href = \"" + wstring_to_utf8(this->_linkurl)
            + "\" class=\"refentry\">" + wstring_to_utf8(this->_linkname)
            + "</a> &#8212; <span class = \"refentry-description\">"
            + wstring_to_utf8(this->_description) + "</span>" + HTML_LI_OUT_TAG + "\n";
    } else {
        utf8stream = utf8stream + HTML_LI_IN_TAG + "<a href = \"" + wstring_to_utf8(this->_linkurl)
            + "\" class=\"refentry\">" + wstring_to_utf8(this->_linkname) + HTML_LI_OUT_TAG + "\n";
    }
    return true;
}
//=============================================================================
bool
XmlDocChapterRefItem::writeAsMarkdown(std::string& utf8stream)
{
    if (!this->_description.empty()) {
        utf8stream = utf8stream + "* " + "[" + wstring_to_utf8(this->_linkname) + "]" + "("
            + wstring_to_utf8(this->_linkurl) + ")" + " - " + wstring_to_utf8(this->_description)
            + "\n";
    } else {
        utf8stream = utf8stream + wstring_to_utf8(this->_linkurl) + " "
            + wstring_to_utf8(this->_linkname) + "\n";
    }
    return true;
}
//=============================================================================
}
//=============================================================================
