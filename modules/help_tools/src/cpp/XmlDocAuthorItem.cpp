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
#include "XmlDocAuthorItem.hpp"

#include <utility>
#include "HtmlTags.hpp"
#include "XmlDocumentTags.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
XmlDocAuthorItem::XmlDocAuthorItem(std::wstring author) : _author(std::move(author)){};
//=============================================================================
XmlDocAuthorItem::~XmlDocAuthorItem() { this->_author = L""; }
//=============================================================================
std::wstring
XmlDocAuthorItem::getItemType()
{
    return utf8_to_wstring(AUTHOR_ITEM_TAG);
}
//=============================================================================
bool
XmlDocAuthorItem::writeAsHtml(std::string& utf8stream)
{
    utf8stream
        = utf8stream + HTML_P_IN_TAG + wstring_to_utf8(this->_author) + HTML_P_OUT_TAG + "\n";
    utf8stream = utf8stream + "\n";
    return true;
}
//=============================================================================
bool
XmlDocAuthorItem::writeAsMarkdown(std::string& utf8stream)
{
    utf8stream = utf8stream + wstring_to_utf8(this->_author) + "\n";
    utf8stream = utf8stream + "\n";
    return true;
}
//=============================================================================
}
//=============================================================================
