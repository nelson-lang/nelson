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
#include <boost/algorithm/string.hpp>
#include <boost/filesystem.hpp>

#include "Messages.hpp"
#include "RelativePath.hpp"
#include "XmlDocChapterDescriptionItem.hpp"
#include "XmlDocChapterIndexItem.hpp"
#include "XmlDocChapterNamer.hpp"
#include "XmlDocListOfFiles.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
XmlDocListOfFiles::XmlDocListOfFiles(wstringVector srcFiles, const std::wstring& dstDirectory,
    bool bOverwriteExistingFiles, DOCUMENT_OUTPUT outputTarget)
{
    this->outputTarget = outputTarget;
    this->chapterTitle.clear();
    this->chapterDescription.clear();
    this->moduleName.clear();
    this->lastError.clear();
    this->srcFiles = std::move(srcFiles);
    this->sectionUpName.clear();
    this->sectionUpUrl.clear();
    if (boost::algorithm::ends_with(dstDirectory, L"\\")
        || boost::algorithm::ends_with(dstDirectory, L"/")) {
        std::wstring modifiedPath = dstDirectory;
        modifiedPath.pop_back();
        this->dstDirectory = modifiedPath;
        this->chapterResultFilename = XmlDocChapterNamer(modifiedPath);
    } else {
        this->dstDirectory = dstDirectory;
        this->chapterResultFilename = XmlDocChapterNamer(dstDirectory);
    }
    this->bOverwriteExistingFiles = bOverwriteExistingFiles;
}
//=============================================================================
XmlDocListOfFiles::~XmlDocListOfFiles()
{
    this->chapterTitle.clear();
    this->chapterDescription.clear();
    this->moduleName.clear();
    this->lastError.clear();
    this->srcFiles.clear();
    this->dstDirectory.clear();
    this->bOverwriteExistingFiles = false;
    this->chapterResultFilename.clear();
    this->clearItems();
    this->sectionUpName.clear();
    this->sectionUpUrl.clear();
}
//=============================================================================
void
XmlDocListOfFiles::clearItems()
{
    for (auto& xmlItem : this->xmlItems) {
        XmlDocDocument* pItem = (XmlDocDocument*)xmlItem;
        if (pItem) {
            delete pItem;
            pItem = nullptr;
        }
    }
    this->xmlItems.clear();
}
//=============================================================================
bool
XmlDocListOfFiles::read()
{
    bool haveChapterFile = false;
    std::wstring chapterFilename = L"";
    for (auto& srcFile : this->srcFiles) {
        bool b = boost::algorithm::ends_with(srcFile, L"chapter.xml");
        if (b) {
            if (!chapterFilename.empty()) {
                this->lastError = _W("multiple chapter.xml definition.");
                return false;
            }
            chapterFilename = srcFile;
            haveChapterFile = true;
        }
    }
    for (auto& srcFile : this->srcFiles) {
        bool b = boost::algorithm::ends_with(srcFile, L"chapter.xml");
        if (!b) {
            XmlDocDocument* xmlDoc = nullptr;
            try {
                xmlDoc = new XmlDocDocument(srcFile, this->sectionUpName, this->dstDirectory,
                    this->bOverwriteExistingFiles, this->outputTarget);
            } catch (std::bad_alloc&) {
                this->lastError = ERROR_MEMORY_ALLOCATION;
                this->clearItems();
                return false;
            }
            if (xmlDoc->readFile()) {
                xmlItems.push_back(xmlDoc);
            } else {
                wstringVector errors = xmlDoc->getError();
                this->lastError = _W("Error in file:") + L"\n" + srcFile;
                delete xmlDoc;
                xmlDoc = nullptr;
                if (!errors.empty()) {
                    this->lastError = this->lastError + L"\n" + errors[0];
                }
                this->clearItems();
                return false;
            }
        }
    }
    if (!haveChapterFile) {
        this->lastError = _W("chapter.xml file is missing.");
        this->clearItems();
        return false;
    } else {
        XmlDocDocument* xmlDoc = nullptr;
        try {
            xmlDoc = new XmlDocDocument(chapterFilename, this->sectionUpName, this->dstDirectory,
                this->bOverwriteExistingFiles, this->outputTarget);
        } catch (std::bad_alloc&) {
            this->lastError = ERROR_MEMORY_ALLOCATION;
            this->clearItems();
            return false;
        }
        if (xmlDoc->readFile()) {
            if (xmlDoc->isChapterDocument()) {
                this->moduleName = xmlDoc->getModuleName();
                boost::container::vector<XmlDocGenericItem*> items
                    = xmlDoc->getXmlDocGenericItems();
                XmlDocChapterIndexItem* indexItem = new XmlDocChapterIndexItem();
                for (auto& xmlItem : xmlItems) {
                    bool bSuccess;
                    indexItem->append(xmlItem->getPageTitle(),
                        RelativePath(dstDirectory, xmlItem->getDestinationFile(), bSuccess),
                        xmlItem->getPageDescription());
                }
                items.push_back(indexItem);
                std::wstring htmlFilenameDestination = L"";
                if (!this->moduleName.empty()) {
                    if (this->outputTarget == DOCUMENT_OUTPUT::MARKDOWN) {
                        htmlFilenameDestination = dstDirectory + L"/README.md";
                    } else {
                        htmlFilenameDestination
                            = dstDirectory + L"/chapter_" + this->moduleName + L".html";
                    }
                    this->chapterResultFilename = htmlFilenameDestination;
                } else {
                    htmlFilenameDestination = this->chapterResultFilename;
                }
                XmlDocDocument* xmlChapterDoc = new XmlDocDocument(items, chapterFilename,
                    htmlFilenameDestination, this->bOverwriteExistingFiles, this->outputTarget);
                xmlItems.push_back(xmlChapterDoc);
                this->chapterTitle = xmlDoc->getChapter();
            } else {
                this->lastError = _W("chapter.xml is missing.");
                delete xmlDoc;
                xmlDoc = nullptr;
                this->clearItems();
                return false;
            }
        } else {
            wstringVector errors = xmlDoc->getError();
            this->lastError = _W("Error in file:") + L"\n" + chapterFilename + L"\n";
            delete xmlDoc;
            xmlDoc = nullptr;
            if (!errors.empty()) {
                this->lastError = this->lastError + L"\n" + errors[0];
            }
            this->clearItems();
            return false;
        }
    }
    for (size_t k = 0; k < xmlItems.size(); k++) {
        if (k > 0) {
            std::wstring linkName = xmlItems[k - 1]->getPageTitle();
            bool bRes;
            std::wstring linkUrl
                = RelativePath(dstDirectory, xmlItems[k - 1]->getDestinationFile(), bRes);
            xmlItems[k]->setPreviousPageLink(linkName, linkUrl);
        }
        if (!this->chapterTitle.empty()) {
            bool bRes;
            std::wstring linkUrl = L"";
            boost::filesystem::path destPath(this->dstDirectory);
            if (this->outputTarget == DOCUMENT_OUTPUT::MARKDOWN) {
                if (!this->moduleName.empty()) {
                    destPath = destPath / (L"README.md");
                } else {
                    destPath = destPath / L"chapter.md";
                }
            } else {
                if (!this->moduleName.empty()) {
                    destPath = destPath / (L"chapter_" + this->moduleName + L".html");
                } else {
                    destPath = destPath / L"chapter.html";
                }
            }
            linkUrl = RelativePath(dstDirectory, destPath.generic_wstring(), bRes);
            xmlItems[k]->setIndexPageLink(this->chapterTitle, linkUrl);
        }
        if (k < xmlItems.size() - 1) {
            std::wstring linkName = xmlItems[k + 1]->getPageTitle();
            bool bRes;
            std::wstring linkUrl
                = RelativePath(dstDirectory, xmlItems[k + 1]->getDestinationFile(), bRes);
            xmlItems[k]->setNextPageLink(linkName, linkUrl);
        }
    }
    // last element is the chapter page
    std::wstring linkName = xmlItems[0]->getPageTitle();
    bool bRes;
    std::wstring linkUrl = RelativePath(dstDirectory, xmlItems[0]->getDestinationFile(), bRes);
    xmlItems[xmlItems.size() - 1]->setNextPageLink(linkName, linkUrl);
    if (!sectionUpName.empty()) {
        xmlItems[xmlItems.size() - 1]->setPreviousPageLink(sectionUpName, sectionUpUrl);
    }
    return true;
}
//=============================================================================
bool
XmlDocListOfFiles::writeAsHtml()
{
    for (auto& xmlItem : this->xmlItems) {
        if (xmlItem) {
            xmlItem->writeAsHtml();
        }
    }
    return true;
}
//=============================================================================
bool
XmlDocListOfFiles::writeAsMarkdown()
{
    for (auto& xmlItem : this->xmlItems) {
        if (xmlItem) {
            xmlItem->writeAsMarkdown();
        }
    }
    return true;
}
//=============================================================================
std::wstring
XmlDocListOfFiles::getLastError()
{
    return this->lastError;
}
//=============================================================================
std::wstring
XmlDocListOfFiles::getGeneratedChapterFilename()
{
    return this->chapterResultFilename;
}
//=============================================================================
std::wstring
XmlDocListOfFiles::getChapterTitle()
{
    return this->chapterTitle;
}
//=============================================================================
std::wstring
XmlDocListOfFiles::getModuleName()
{
    return this->moduleName;
}
//=============================================================================
void
XmlDocListOfFiles::getIndex(wstringVector& names, wstringVector& urls, wstringVector& descriptions)
{
    bool bSuccess = false;
    for (auto& xmlItem : xmlItems) {
        if (xmlItem->getPageTitle() != L"") {
            names.push_back(xmlItem->getPageTitle());
            urls.push_back(RelativePath(dstDirectory, xmlItem->getDestinationFile(), bSuccess));
            descriptions.push_back(xmlItem->getPageDescription());
        }
    }
}
//=============================================================================
void
XmlDocListOfFiles::setUpSection(const std::wstring& sectionName, const std::wstring& sectionUrl)
{
    this->sectionUpName = sectionName;
    this->sectionUpUrl = sectionUrl;
}
//=============================================================================
}
//=============================================================================
