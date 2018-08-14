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

#include "Error.hpp"
#include "ImageTagHelpers.hpp"
#include "XmlDocImageItem.hpp"
#include "XmlDocumentTags.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
XmlDocImageItem::XmlDocImageItem(const std::wstring& tag) { this->tag = tag; }
//=============================================================================
XmlDocImageItem::~XmlDocImageItem() { tag = L""; }
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
    Nelson::copyImage(this->imageSource, this->imageDestination);
    utf8stream = utf8stream + "\n";
    utf8stream = utf8stream + wstring_to_utf8(tag) + "\n";
    utf8stream = utf8stream + "\n";
    return true;
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
        boost::replace_all(tag, oldPath, newfilename);
        imageSource = newPath;
        imageDestination = this->destDirectory + L"/" + newfilename;
    } else {
        Error(_W("File does not exist:") + L" " + oldPath);
    }
}
//=============================================================================
void
XmlDocImageItem::setDirectories(const std::wstring& srcDirectory, const std::wstring& destDirectory)
{
    if (boost::algorithm::ends_with(srcDirectory, L"/")
        || boost::algorithm::ends_with(srcDirectory, L"\\")) {
        this->srcDirectory = srcDirectory;
    } else {
        this->srcDirectory = srcDirectory + L"/";
    }
    if (boost::algorithm::ends_with(destDirectory, L"/")
        || boost::algorithm::ends_with(destDirectory, L"\\")) {
        this->destDirectory = destDirectory;
    } else {
        this->destDirectory = destDirectory + L"/";
    }
}
//=============================================================================
}
//=============================================================================
