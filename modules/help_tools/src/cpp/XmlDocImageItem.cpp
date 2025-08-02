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
#include "Error.hpp"
#include "i18n.hpp"
#include "ImageTagHelpers.hpp"
#include "XmlDocImageItem.hpp"
#include "XmlDocumentTags.hpp"
#include "characters_encoding.hpp"
#include "FileSystemWrapper.hpp"
//=============================================================================
namespace Nelson {
XmlDocImageItem::XmlDocImageItem(const std::wstring& tag) { this->tag.assign(tag); }
//=============================================================================
XmlDocImageItem::~XmlDocImageItem() { tag.clear(); }
//=============================================================================
std::wstring
XmlDocImageItem::getItemType()
{
    return utf8_to_wstring(IMAGE_TAG);
}
//=============================================================================
bool
XmlDocImageItem::writeAsHtml(std::string& utf8stream)
{
    return writeAsMarkdown(utf8stream);
}
//=============================================================================
bool
XmlDocImageItem::writeAsMarkdown(std::string& utf8stream)
{
    Nelson::copyImage(this->imageSource, this->imageDestination);
    utf8stream = utf8stream + "\n";
    utf8stream = utf8stream + wstring_to_utf8(tag) + "\n";
    utf8stream = utf8stream + "\n";
    return true;
}
//=============================================================================
void
XmlDocImageItem::findImage()
{
    if (!isValidImageTag(tag)) {
        Error(_W("Tag malformed."));
    }
    std::wstring newPath;
    std::wstring oldPath;
    if (parseImageTag(tag, srcDirectory, oldPath, newPath)) {
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
        StringHelpers::replace_all(tag, oldPath, newfilename);
        imageSource.assign(newPath);
        imageDestination = this->destDirectory + L"/" + newfilename;
    } else {
        Error(_W("File does not exist:") + L" " + oldPath);
    }
}
//=============================================================================
void
XmlDocImageItem::setDirectories(const std::wstring& srcDirectory, const std::wstring& destDirectory)
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
