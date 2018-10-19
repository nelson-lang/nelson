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
#include "XmlDocDescriptionItem.hpp"
#include "Error.hpp"
#include "HtmlTags.hpp"
#include "ImageTagHelpers.hpp"
#include "Types.hpp"
#include "XmlDocumentTags.hpp"
#include "characters_encoding.hpp"
#include "i18n.hpp"
#include <boost/algorithm/string.hpp>
#include <boost/algorithm/string/predicate.hpp>
#include <boost/filesystem.hpp>
//=============================================================================
namespace Nelson {
//=============================================================================
XmlDocDescriptionItem::XmlDocDescriptionItem(std::wstring description)
{
    this->_description = description;
    this->imagesTag.clear();
    this->imagesSource.clear();
    this->imagesDestination.clear();
    this->srcDirectory = L"";
    this->destDirectory = L"";
    this->haveImages = this->checkImageTag();
}
//=============================================================================
XmlDocDescriptionItem::~XmlDocDescriptionItem()
{
    this->_description = L"";
    this->imagesTag.clear();
    this->imagesSource.clear();
    this->imagesDestination.clear();
    this->haveImages = false;
    this->srcDirectory = L"";
    this->destDirectory = L"";
}
//=============================================================================
void
XmlDocDescriptionItem::setValue(std::wstring value)
{
    this->_description = value;
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
    if (!boost::algorithm::starts_with(_description, HTML_P_IN_TAG)) {
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
    for (size_t k = 0; k < imagesTag.size(); k++) {
        std::wstring tag = imagesTag[k];
        std::wstring oldPath;
        std::wstring newPath;
        if (parseImageTag(tag, this->srcDirectory, oldPath, newPath)) {
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
            boost::replace_all(this->_description, imagesTag[k], tag);
            imagesTag[k] = tag;
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
XmlDocDescriptionItem::setDirectories(std::wstring srcDirectory, std::wstring destDirectory)
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
