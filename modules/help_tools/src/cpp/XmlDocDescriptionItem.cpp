//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "XmlDocDescriptionItem.hpp"
#include "Error.hpp"
#include "HtmlTags.hpp"
#include "ImageTagHelpers.hpp"
#include "Types.hpp"
#include "XmlDocumentTags.hpp"
#include "characters_encoding.hpp"
#include "i18n.hpp"
#include "FileSystemWrapper.hpp"
#include "StringHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
XmlDocDescriptionItem::XmlDocDescriptionItem(const std::wstring& description)
{
    this->_description = description;
    this->imagesTag.clear();
    this->imagesSource.clear();
    this->imagesDestination.clear();
    this->srcDirectory.clear();
    this->destDirectory.clear();
    this->haveImages = this->checkImageTag();
}
//=============================================================================
XmlDocDescriptionItem::~XmlDocDescriptionItem()
{
    this->_description.clear();
    this->imagesTag.clear();
    this->imagesSource.clear();
    this->imagesDestination.clear();
    this->haveImages = false;
    this->srcDirectory.clear();
    this->destDirectory.clear();
}
//=============================================================================
void
XmlDocDescriptionItem::setValue(const std::wstring& value)
{
    this->_description.assign(value);
}
//=============================================================================
std::wstring
XmlDocDescriptionItem::getValue()
{
    return this->_description;
}
//=============================================================================
std::wstring
XmlDocDescriptionItem::getItemType()
{
    return utf8_to_wstring(DESCRIPTION_TAG);
}
//=============================================================================
bool
XmlDocDescriptionItem::writeAsHtml(std::string& utf8stream)
{
    if (imagesSource.size()) {
        copyImages(imagesSource, imagesDestination);
    }
    utf8stream = utf8stream + HTML_H3_IN_TAG + _("Description") + HTML_H3_OUT_TAG + "\n";
    utf8stream = utf8stream + HTML_HR_OUT_TAG + "\n";
    utf8stream = utf8stream + "\n";
    if (!StringHelpers::starts_with(_description, utf8_to_wstring(HTML_P_IN_TAG))) {
        utf8stream = utf8stream + HTML_P_IN_TAG + HTML_P_OUT_TAG + "\n";
    }
    utf8stream = utf8stream + wstring_to_utf8(_description) + "\n";
    utf8stream = utf8stream + "\n";
    return true;
}
//=============================================================================
bool
XmlDocDescriptionItem::writeAsMarkdown(std::string& utf8stream)
{
    if (imagesSource.size()) {
        copyImages(imagesSource, imagesDestination);
    }
    utf8stream = utf8stream + "## " + _("Description") + "\n";
    utf8stream = utf8stream + "\n";
    utf8stream = utf8stream + wstring_to_utf8(_description) + "\n";
    utf8stream = utf8stream + "\n";
    return true;
}
//=============================================================================
bool
XmlDocDescriptionItem::checkImageTag()
{
    if (!this->_description.empty()) {
        return isValidImageTag(_description);
    }
    return false;
}
//=============================================================================
void
XmlDocDescriptionItem::replaceImageTag()
{
    // extract path
    for (auto& k : imagesTag) {
        std::wstring tag = k;
        std::wstring oldPath;
        std::wstring newPath;
        if (parseImageTag(tag, this->srcDirectory, oldPath, newPath)) {
            std::wstring filename = L"";
            std::wstring extension = L"";
            FileSystemWrapper::Path absolutePath = oldPath;
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
            StringHelpers::replace_all(tag, oldPath, newfilename);
            StringHelpers::replace_all(this->_description, k, tag);
            k = tag;
            imagesSource.push_back(newPath);
            imagesDestination.push_back(this->destDirectory + L"/" + newfilename);
        } else {
            Error(_W("File does not exist:") + L" " + oldPath);
        }
    }
}
//=============================================================================
void
XmlDocDescriptionItem::searchImageTag()
{
    if (this->haveImages) {
        this->imagesTag.clear();
        if (findImageTag(this->_description, this->imagesTag)) {
            replaceImageTag();
        }
    }
}
//=============================================================================
void
XmlDocDescriptionItem::setDirectories(
    const std::wstring& srcDirectory, const std::wstring& destDirectory)
{
    if (StringHelpers::ends_with(srcDirectory, L"/")
        || StringHelpers::ends_with(srcDirectory, L"\\")) {
        this->srcDirectory = srcDirectory;
    } else {
        this->srcDirectory = srcDirectory + L"/";
    }
    if (StringHelpers::ends_with(destDirectory, L"/")
        || StringHelpers::ends_with(destDirectory, L"\\")) {
        this->destDirectory = destDirectory;
    } else {
        this->destDirectory = destDirectory + L"/";
    }
}
//=============================================================================
}
//=============================================================================
