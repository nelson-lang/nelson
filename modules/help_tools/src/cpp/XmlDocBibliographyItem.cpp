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
#include "XmlDocBibliographyItem.hpp"
#include "HtmlTags.hpp"
#include "XmlDocumentTags.hpp"
#include "characters_encoding.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
XmlDocBibliographyItem::XmlDocBibliographyItem(std::wstring bibliography)
{
    this->_bibliography = bibliography;
}
//=============================================================================
XmlDocBibliographyItem::~XmlDocBibliographyItem() { this->_bibliography = L""; }
//=============================================================================
std::wstring
XmlDocBibliographyItem::getItemType()
{
    return utf8_to_wstring(BIBLIOGRAPHY_TAG);
}
//=============================================================================
bool
XmlDocBibliographyItem::writeAsHtml(std::string& utf8stream)
{
    if (!this->_bibliography.empty()) {
        utf8stream = utf8stream + HTML_H3_IN_TAG + _("Bibliography") + HTML_H3_OUT_TAG + "\n";
        utf8stream = utf8stream + HTML_HR_OUT_TAG + "\n";
        utf8stream = utf8stream + wstring_to_utf8(this->_bibliography) + "\n";
        utf8stream = utf8stream + "\n";
    }
    return true;
}
//=============================================================================
bool
XmlDocBibliographyItem::writeAsMarkdown(std::string& utf8stream)
{
    if (!this->_bibliography.empty()) {
        utf8stream = utf8stream + _("Bibliography") + "\n";
        utf8stream = utf8stream + "\n";
        utf8stream = utf8stream + wstring_to_utf8(this->_bibliography) + "\n";
        utf8stream = utf8stream + "\n";
    }
    return true;
}
//=============================================================================
}
//=============================================================================
