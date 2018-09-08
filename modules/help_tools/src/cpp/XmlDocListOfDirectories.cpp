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
#include "XmlDocListOfDirectories.hpp"
#include "RelativePath.hpp"
#include "XmlDocTitleItem.hpp"
#include <boost/algorithm/string.hpp>
//=============================================================================
namespace Nelson {
//=============================================================================
XmlDocListOfDirectories::XmlDocListOfDirectories(wstringVector srcDirectories,
    std::wstring dstDirectory, std::wstring mainTitle, bool bOverwriteExistingFiles,
    DOCUMENT_OUTPUT outputTarget)
{
    this->mainTitle = mainTitle;
    this->outputTarget = outputTarget;
    this->srcDirectories = srcDirectories;
    this->dstDirectory = dstDirectory;
    if (boost::algorithm::ends_with(dstDirectory, L"/")
        || boost::algorithm::ends_with(dstDirectory, L"\\")) {
        this->dstDirectory.pop_back();
    }
    this->bOverwriteExistingFiles = bOverwriteExistingFiles;
    this->lastError = L"";
    for (size_t k = 0; k < srcDirectories.size(); k++) {
        XmlDocDirectory* xmlDirectory;
        try {
            xmlDirectory = new XmlDocDirectory(srcDirectories[k], this->dstDirectory,
                this->bOverwriteExistingFiles, this->outputTarget);
        } catch (const std::bad_alloc&) {
            xmlDirectory = nullptr;
        }
        if (xmlDirectory) {
            bool bSuccess;
            xmlDirectory->setUpSection(this->mainTitle,
                RelativePath(this->dstDirectory, this->dstDirectory + L"/index.html", bSuccess));
            itemsDirectories.push_back(xmlDirectory);
        }
    }
    this->mainIndex = nullptr;
}
//=============================================================================
XmlDocListOfDirectories::~XmlDocListOfDirectories()
{
    this->srcDirectories.clear();
    this->dstDirectory = L"";
    this->lastError = L"";
    this->bOverwriteExistingFiles = false;
    this->clearItems();
    this->outputTarget = DOCUMENT_OUTPUT::HMTL;
    if (this->mainIndex) {
        delete this->mainIndex;
        this->mainIndex = nullptr;
    }
}
//=============================================================================
void
XmlDocListOfDirectories::clearItems()
{
    for (size_t k = 0; k < this->itemsDirectories.size(); k++) {
        XmlDocDirectory* pItem = (XmlDocDirectory*)this->itemsDirectories[k];
        if (pItem) {
            delete pItem;
            pItem = nullptr;
        }
    }
    this->itemsDirectories.clear();
}
//=============================================================================
bool
XmlDocListOfDirectories::read()
{
    for (size_t k = 0; k < itemsDirectories.size(); k++) {
        if (!itemsDirectories[k]->read()) {
            this->lastError = itemsDirectories[k]->getLastError();
            return false;
        }
    }
    try {
        mainIndex = new XmlDocMainIndex(
            this->dstDirectory, this->mainTitle, this->getOutputHelpBasename(), outputTarget);
    } catch (const std::bad_alloc&) {
        mainIndex = nullptr;
    }
    if (mainIndex) {
        for (size_t k = 0; k < itemsDirectories.size(); k++) {
            std::wstring titleChapter = itemsDirectories[k]->getChapterTitle();
            std::wstring filenameChapter = itemsDirectories[k]->getGeneratedChapterFilename();
            wstringVector names;
            wstringVector urls;
            wstringVector descriptions;
            itemsDirectories[k]->getIndex(names, urls, descriptions);
            mainIndex->appendSection(titleChapter, filenameChapter, names, urls, descriptions);
        }
    }
    return true;
}
//=============================================================================
bool
XmlDocListOfDirectories::writeAsMarkdown()
{
    for (size_t k = 0; k < itemsDirectories.size(); k++) {
        if (!itemsDirectories[k]->writeAsMarkdown()) {
            this->lastError = itemsDirectories[k]->getLastError();
            return false;
        }
    }
    if (mainIndex) {
        mainIndex->writeAsMarkdown();
    }
    return true;
}
//=============================================================================
bool
XmlDocListOfDirectories::writeAsHtml()
{
    for (size_t k = 0; k < itemsDirectories.size(); k++) {
        if (!itemsDirectories[k]->writeAsHtml()) {
            this->lastError = itemsDirectories[k]->getLastError();
            return false;
        }
    }
    if (mainIndex) {
        mainIndex->writeAsHtml();
    }
    return true;
}
//=============================================================================
std::wstring
XmlDocListOfDirectories::getLastError()
{
    return this->lastError;
}
//=============================================================================
void
XmlDocListOfDirectories::getIndex(
    wstringVector& names, wstringVector& urls, wstringVector& descriptions)
{
    for (size_t k = 0; k < itemsDirectories.size(); k++) {
        itemsDirectories[k]->getIndex(names, urls, descriptions);
    }
}
//=============================================================================
std::wstring
XmlDocListOfDirectories::getOutputHelpBasename()
{
    std::wstring output = L"";
    if (this->outputTarget == DOCUMENT_OUTPUT::QT_HELP) {
        if (itemsDirectories.size() > 0) {
            output = itemsDirectories[0]->getModuleName();
        }
    }
    return output;
}
//=============================================================================
}
//=============================================================================
