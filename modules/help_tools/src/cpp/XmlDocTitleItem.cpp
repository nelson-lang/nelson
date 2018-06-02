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
#include "XmlDocTitleItem.hpp"
#include "HtmlTags.hpp"
#include "XmlDocumentTags.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
XmlDocTitleItem::XmlDocTitleItem(std::wstring title) { this->_title = title; }
//=============================================================================
XmlDocTitleItem::~XmlDocTitleItem() { this->_title = L""; }
//=============================================================================
void
XmlDocTitleItem::setValue(std::wstring value)
{
    this->_title = value;
}
//=============================================================================
std::wstring
XmlDocTitleItem::getValue()
{
    return this->_title;
}
//=============================================================================
std::wstring
XmlDocTitleItem::getItemType()
{
    return utf8_to_wstring(TITLE_TAG);
}
//=============================================================================
bool
XmlDocTitleItem::writeAsHtml(std::string& utf8stream)
{
    utf8stream = utf8stream + HTML_H1_REFNAME_IN_TAG + wstring_to_utf8(this->_title)
        + HTML_H1_OUT_TAG + "\n";
    utf8stream = utf8stream + HTML_HR_OUT_TAG + "\n";
    utf8stream = utf8stream + "\n";
    return true;
}
//=============================================================================
bool
XmlDocTitleItem::writeHeaderAsHtml(std::string& utf8stream)
{
    utf8stream = utf8stream + "\t"
        + "<meta http-equiv=\"Content-Type\" content = \"text/html; charset=utf-8\" />" + "\n";
    utf8stream = utf8stream + "\t" + HTML_TITLE_IN_TAG + wstring_to_utf8(this->_title)
        + HTML_TITLE_OUT_TAG + "\n";
    utf8stream = utf8stream + "\n";
    return true;
}
//=============================================================================
bool
XmlDocTitleItem::writeAsMarkdown(std::string& utf8stream)
{
    utf8stream = utf8stream + wstring_to_utf8(this->_title) + "\n";
    utf8stream = utf8stream + "\n";
    return true;
}
//=============================================================================
bool
XmlDocTitleItem::writeHeaderAsMarkdown(std::string& utf8stream)
{
    utf8stream = utf8stream + "\n";
    utf8stream = utf8stream + "# " + wstring_to_utf8(this->_title) + "\n";
    utf8stream = utf8stream + "\n";
    return true;
}
//=============================================================================
}
//=============================================================================
