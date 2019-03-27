//=============================================================================
// Copyright (c) 2016-2019 Allan CORNET (Nelson)
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
#include "XmlDocShortDescriptionItem.hpp"
#include "HtmlTags.hpp"
#include "XmlDocumentTags.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
XmlDocShortDescriptionItem::XmlDocShortDescriptionItem(const std::wstring& description)
{
    this->_description.assign(description);
}
//=============================================================================
XmlDocShortDescriptionItem::~XmlDocShortDescriptionItem() { this->_description.clear(); }
//=============================================================================
void
XmlDocShortDescriptionItem::setValue(const std::wstring& value)
{
    this->_description.assign(value);
}
//=============================================================================
std::wstring
XmlDocShortDescriptionItem::getValue()
{
    return this->_description;
}
//=============================================================================
std::wstring
XmlDocShortDescriptionItem::getItemType()
{
    return utf8_to_wstring(SHORT_DESCRIPTION_TAG);
}
//=============================================================================
bool
XmlDocShortDescriptionItem::writeAsHtml(std::string& utf8stream)
{
    utf8stream
        = utf8stream + HTML_H3_IN_TAG + wstring_to_utf8(_description) + HTML_H3_OUT_TAG + "\n";
    utf8stream = utf8stream + HTML_HR_OUT_TAG + "\n";
    utf8stream = utf8stream + "\n";
    return true;
}
//=============================================================================
bool
XmlDocShortDescriptionItem::writeAsMarkdown(std::string& utf8stream)
{
    utf8stream = utf8stream + wstring_to_utf8(_description) + "\n";
    utf8stream = utf8stream + "\n";
    return true;
}
//=============================================================================
}
//=============================================================================
