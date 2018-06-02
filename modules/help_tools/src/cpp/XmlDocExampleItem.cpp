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
#if _MSC_VER
#define _SCL_SECURE_NO_WARNINGS
#endif
#include <boost/algorithm/string.hpp>
#include <boost/filesystem.hpp>

#include "HtmlTags.hpp"
#include "ImageTagHelpers.hpp"
#include "XmlDocExampleItem.hpp"
#include "XmlDocumentTags.hpp"
#include "characters_encoding.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
XmlDocExampleItem::XmlDocExampleItem(const std::wstring& type, const std::wstring& description,
    const std::wstring& data, const std::wstring& imageTag, DOCUMENT_OUTPUT outputTarget)
{
    this->_type = type;
    this->_description = description;
    this->_data = data;
    this->_imageTag = imageTag;
    this->_outputTarget = outputTarget;
    this->_srcDirectory = L"";
    this->_dstDirectory = L"";
    this->_imageSource = L"";
}
//=============================================================================
XmlDocExampleItem::~XmlDocExampleItem()
{
    this->_type = L"";
    this->_description = L"";
    this->_data = L"";
    this->_imageTag = L"";
    this->_outputTarget = DOCUMENT_OUTPUT::HMTL;
    this->_srcDirectory = L"";
    this->_dstDirectory = L"";
}
//=============================================================================
std::wstring
XmlDocExampleItem::getType()
{
    return this->_type;
}
//=============================================================================
std::wstring
XmlDocExampleItem::getDescription()
{
    return this->_description;
}
//=============================================================================
std::wstring
XmlDocExampleItem::getData()
{
    return this->_data;
}
//=============================================================================
std::wstring
XmlDocExampleItem::getImageTag()
{
    return this->_imageTag;
}
//=============================================================================
std::wstring
XmlDocExampleItem::getItemType()
{
    return utf8_to_wstring(EXAMPLE_ITEM_TAG);
}
//=============================================================================
bool
XmlDocExampleItem::isNelsonExample()
{
    return (boost::iequals(this->_type, utf8_to_wstring(NELSON_EXAMPLE_TYPE)));
}
//=============================================================================
bool
XmlDocExampleItem::writeAsMarkdown(std::string& utf8stream)
{
    if (!this->getDescription().empty()) {
        utf8stream = utf8stream + wstring_to_utf8(this->getDescription()) + "\n";
    }
    if (this->isNelsonExample()) {
        // utf8stream = utf8stream + "```nelson" + "\n";
        // need to extend highlighter.js
        utf8stream = utf8stream + "```matlab" + "\n";
    } else {
        utf8stream = utf8stream + "```" + wstring_to_utf8(this->getType()) + "\n";
    }
    utf8stream = utf8stream + wstring_to_utf8(this->getData()) + "\n";
    utf8stream = utf8stream + "```" + "\n";
    if (!this->getImageTag().empty()) {
        std::wstring oldPath;
        std::wstring newPath;
        if (parseImageTag(this->_imageTag, this->_srcDirectory, oldPath, newPath)) {
            std::wstring filename = L"";
            std::wstring extension = L"";
            boost::filesystem::path absolutePath = oldPath;
            if (absolutePath.has_filename()) {
                filename = absolutePath.stem().generic_wstring();
            }
            if (absolutePath.has_extension()) {
                extension = absolutePath.extension().generic_wstring();
            }
            std::wstring crc = crcFile(newPath);
            std::wstring newfilename;
            if (crc == L"") {
                newfilename = filename + extension;
            } else {
                newfilename = filename + L"_" + crc + extension;
            }
            boost::replace_all(this->_imageTag, oldPath, newfilename);
            _imageSource = newPath;
            _imageDestination = this->_dstDirectory + L"/" + newfilename;
            Nelson::copyImage(_imageSource, _imageDestination);
        }
        utf8stream = utf8stream + wstring_to_utf8(this->getImageTag()) + "\n";
    }
    return true;
}
//=============================================================================
bool
XmlDocExampleItem::writeAsHtml(std::string& utf8stream)
{
    if (!this->getDescription().empty()) {
        utf8stream = utf8stream + HTML_P_IN_TAG + wstring_to_utf8(this->getDescription())
            + HTML_P_OUT_TAG + "\n";
    }
    utf8stream = utf8stream + HTML_PRE_IN_TAG + "\n";
    utf8stream = utf8stream + "<code class = \"" + wstring_to_utf8(this->getType()) + "\">";
    utf8stream = utf8stream + wstring_to_utf8(this->getData()) + "\n";
    utf8stream = utf8stream + HTML_CODE_OUT_TAG + "\n";
    utf8stream = utf8stream + HTML_PRE_OUT_TAG + "\n";
    if (!this->getImageTag().empty()) {
        std::wstring oldPath;
        std::wstring newPath;
        if (parseImageTag(this->_imageTag, this->_srcDirectory, oldPath, newPath)) {
            std::wstring filename = L"";
            std::wstring extension = L"";
            boost::filesystem::path absolutePath = oldPath;
            if (absolutePath.has_filename()) {
                filename = absolutePath.stem().generic_wstring();
            }
            if (absolutePath.has_extension()) {
                extension = absolutePath.extension().generic_wstring();
            }
            std::wstring crc = crcFile(newPath);
            std::wstring newfilename;
            if (crc == L"") {
                newfilename = filename + extension;
            } else {
                newfilename = filename + L"_" + crc + extension;
            }
            boost::replace_all(this->_imageTag, oldPath, newfilename);
            _imageSource = newPath;
            _imageDestination = this->_dstDirectory + L"/" + newfilename;
            Nelson::copyImage(_imageSource, _imageDestination);
        }
        if (this->_outputTarget == DOCUMENT_OUTPUT::QT_HELP) {
            utf8stream = utf8stream + HTML_P_IN_TAG + wstring_to_utf8(this->getImageTag())
                + HTML_P_OUT_TAG + "\n";
        } else {
            utf8stream = utf8stream + wstring_to_utf8(this->getImageTag()) + "\n";
        }
    }
    return true;
}
//=============================================================================
void
XmlDocExampleItem::setDirectories(
    const std::wstring& srcDirectory, const std::wstring& dstDirectory)
{
    this->_srcDirectory = srcDirectory;
    this->_dstDirectory = dstDirectory;
}
//=============================================================================
}
//=============================================================================
