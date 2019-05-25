//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
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
#include "XmlDocParamOutputItem.hpp"
#include "HtmlTags.hpp"
#include "XmlDocumentTags.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
XmlDocParamOutputItem::XmlDocParamOutputItem(
    const std::wstring& name, const std::wstring& description)
{
    this->_name.assign(name);
    this->_description.assign(description);
}
//=============================================================================
XmlDocParamOutputItem::~XmlDocParamOutputItem()
{
    this->_name.clear();
    this->_description.clear();
}
//=============================================================================
std::wstring
XmlDocParamOutputItem::getName()
{
    return this->_name;
}
//=============================================================================
std::wstring
XmlDocParamOutputItem::getDescription()
{
    return this->_description;
}
//=============================================================================
std::wstring
XmlDocParamOutputItem::getItemType()
{
    return utf8_to_wstring(PARAM_OUTPUT_TAG);
}
//=============================================================================
bool
XmlDocParamOutputItem::writeAsHtml(std::string& utf8stream)
{
    utf8stream = utf8stream + HTML_DT_IN_TAG + "<span class=\"term\">"
        + wstring_to_utf8(this->_name) + "</span>" + HTML_DT_OUT_TAG + "\n";
    utf8stream = utf8stream + HTML_DD_IN_TAG + "\n";
    utf8stream = utf8stream + "<p class=\"para\">" + wstring_to_utf8(this->_description)
        + HTML_P_OUT_TAG + "\n";
    utf8stream = utf8stream + HTML_DD_OUT_TAG + "\n";
    utf8stream = utf8stream + "\n";
    return true;
}
//=============================================================================
bool
XmlDocParamOutputItem::writeAsMarkdown(std::string& utf8stream)
{
    utf8stream = utf8stream + " - " + wstring_to_utf8(this->_name);
    utf8stream = utf8stream + " - " + wstring_to_utf8(this->_description) + "\n";
    return true;
}
//=============================================================================
}
//=============================================================================
