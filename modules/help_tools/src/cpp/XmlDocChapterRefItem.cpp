//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "XmlDocChapterRefItem.hpp"

#include <utility>
#include "HtmlTags.hpp"
#include "XmlDocumentTags.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
XmlDocChapterRefItem::XmlDocChapterRefItem(
    std::wstring linkname, std::wstring linkurl, std::wstring description)
    : _description(std::move(description))
    , _linkname(std::move(linkname))
    , _linkurl(std::move(linkurl))
{
}
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
            + R"(" class="refentry">)" + wstring_to_utf8(this->_linkname)
            + "</a> &#8212; <span class = \"refentry-description\">"
            + wstring_to_utf8(this->_description) + "</span>" + HTML_LI_OUT_TAG + "\n";
    } else {
        utf8stream = utf8stream + HTML_LI_IN_TAG + "<a href = \"" + wstring_to_utf8(this->_linkurl)
            + R"(" class="refentry">)" + wstring_to_utf8(this->_linkname) + HTML_LI_OUT_TAG + "\n";
    }
    return true;
}
//=============================================================================
bool
XmlDocChapterRefItem::writeAsMarkdown(std::string& utf8stream)
{
    if (!this->_description.empty()) {
        if (!this->_linkname.empty()) {
            utf8stream = utf8stream + "* " + "[" + wstring_to_utf8(this->_linkname) + "]" + "("
                + wstring_to_utf8(this->_linkurl) + ")" + " - "
                + wstring_to_utf8(this->_description) + "\n";
        } else {
            utf8stream = utf8stream + "* " + "[" + wstring_to_utf8(this->_description) + "]" + "("
                + wstring_to_utf8(this->_linkurl) + ")" + "\n";
        }
    } else {
        utf8stream = utf8stream + "* " + "[" + wstring_to_utf8(this->_linkname) + "]" + "("
            + wstring_to_utf8(this->_linkurl) + ")" + "\n";
    }
    return true;
}
//=============================================================================
}
//=============================================================================
