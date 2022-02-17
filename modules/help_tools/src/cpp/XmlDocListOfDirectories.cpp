//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// Alternatively, you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of
// the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this program. If not, see <http://www.gnu.org/licenses/>.
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
    const std::wstring& dstDirectory, const std::wstring& mainTitle, bool bOverwriteExistingFiles,
    DOCUMENT_OUTPUT outputTarget)
    : srcDirectories(srcDirectories), dstDirectory(dstDirectory)
{
    this->mainTitle = mainTitle;
    this->outputTarget = outputTarget;
    if (boost::algorithm::ends_with(dstDirectory, L"/")
        || boost::algorithm::ends_with(dstDirectory, L"\\")) {
        this->dstDirectory.pop_back();
    }
    this->bOverwriteExistingFiles = bOverwriteExistingFiles;
    this->lastError.clear();
    for (auto& srcDirectorie : srcDirectories) {
        XmlDocDirectory* xmlDirectory;
        try {
            xmlDirectory = new XmlDocDirectory(srcDirectorie, this->dstDirectory,
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
    this->dstDirectory.clear();
    this->lastError.clear();
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
    for (auto& itemsDirectorie : this->itemsDirectories) {
        XmlDocDirectory* pItem = (XmlDocDirectory*)itemsDirectorie;
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
    for (auto& itemsDirectorie : itemsDirectories) {
        if (!itemsDirectorie->read()) {
            this->lastError = itemsDirectorie->getLastError();
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
        for (auto& itemsDirectorie : itemsDirectories) {
            std::wstring titleChapter = itemsDirectorie->getChapterTitle();
            std::wstring filenameChapter = itemsDirectorie->getGeneratedChapterFilename();
            wstringVector names;
            wstringVector urls;
            wstringVector descriptions;
            itemsDirectorie->getIndex(names, urls, descriptions);
            mainIndex->appendSection(titleChapter, filenameChapter, names, urls, descriptions);
        }
    }
    return true;
}
//=============================================================================
bool
XmlDocListOfDirectories::writeAsMarkdown()
{
    for (auto& itemsDirectorie : itemsDirectories) {
        if (!itemsDirectorie->writeAsMarkdown()) {
            this->lastError = itemsDirectorie->getLastError();
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
    for (auto& itemsDirectorie : itemsDirectories) {
        if (!itemsDirectorie->writeAsHtml()) {
            this->lastError = itemsDirectorie->getLastError();
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
    for (auto& itemsDirectorie : itemsDirectories) {
        itemsDirectorie->getIndex(names, urls, descriptions);
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
