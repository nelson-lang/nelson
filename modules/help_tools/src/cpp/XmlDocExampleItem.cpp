//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#if _MSC_VER
#define _SCL_SECURE_NO_WARNINGS
#endif
#include "StringHelpers.hpp"
#include "HtmlTags.hpp"
#include "ImageTagHelpers.hpp"
#include "XmlDocExampleItem.hpp"
#include "XmlDocumentTags.hpp"
#include "characters_encoding.hpp"
#include "i18n.hpp"
#include "FileSystemWrapper.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
XmlDocExampleItem::XmlDocExampleItem(const std::wstring& type, const std::wstring& description,
    const std::wstring& data, const std::wstring& imageTag, DOCUMENT_OUTPUT outputTarget)
{
    this->_type.assign(type);
    this->_description.assign(description);
    this->_data.assign(data);
    this->_imageTag.assign(imageTag);
    this->_outputTarget = outputTarget;
    this->_srcDirectory.clear();
    this->_dstDirectory.clear();
    this->_imageSource.clear();
}
//=============================================================================
XmlDocExampleItem::~XmlDocExampleItem()
{
    this->_type.clear();
    this->_description.clear();
    this->_data.clear();
    this->_imageTag.clear();
    this->_outputTarget = DOCUMENT_OUTPUT::HMTL;
    this->_srcDirectory.clear();
    this->_dstDirectory.clear();
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
    return (StringHelpers::iequals(this->_type, utf8_to_wstring(NELSON_EXAMPLE_TYPE)));
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
            FileSystemWrapper::Path absolutePath(oldPath);
            if (absolutePath.has_filename()) {
                filename = absolutePath.stem().generic_wstring();
            }
            if (absolutePath.has_extension()) {
                extension = absolutePath.extension().generic_wstring();
            }
            std::wstring crc = crcFile(newPath);
            std::wstring newfilename;
            if (crc.empty()) {
                newfilename = filename + extension;
            } else {
                newfilename = filename + L"_" + crc + extension;
            }
            StringHelpers::replace_all(this->_imageTag, oldPath, newfilename);
            _imageSource.assign(newPath);
            _imageDestination = this->_dstDirectory + L"/" + newfilename;
            Nelson::copyImage(_imageSource, _imageDestination);
        }
        utf8stream = utf8stream + wstring_to_utf8(this->getImageTag()) + "\n\n";
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
            FileSystemWrapper::Path absolutePath(oldPath);
            if (absolutePath.has_filename()) {
                filename = absolutePath.stem().generic_wstring();
            }
            if (absolutePath.has_extension()) {
                extension = absolutePath.extension().generic_wstring();
            }
            std::wstring crc = crcFile(newPath);
            std::wstring newfilename;
            if (crc.empty()) {
                newfilename = filename + extension;
            } else {
                newfilename = filename + L"_" + crc + extension;
            }
            StringHelpers::replace_all(this->_imageTag, oldPath, newfilename);
            _imageSource.assign(newPath);
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
