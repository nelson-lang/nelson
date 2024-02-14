//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#if _MSC_VER
#define _SCL_SECURE_NO_WARNINGS
#endif
//=============================================================================
#include <fstream>
#include "FileSystemWrapper.hpp"
#include "HtmlTags.hpp"
#include "ImageTagHelpers.hpp"
#include "Localization.hpp"
#include "ModulesManager.hpp"
#include "RelativePath.hpp"
#include "XmlDocAuthorItem.hpp"
#include "XmlDocAuthors.hpp"
#include "XmlDocBibliographyItem.hpp"
#include "XmlDocChapterDescriptionItem.hpp"
#include "XmlDocChapterIndexItem.hpp"
#include "XmlDocChapterItem.hpp"
#include "XmlDocCheckIfLinkExists.hpp"
#include "XmlDocCopyrightItem.hpp"
#include "XmlDocDescriptionItem.hpp"
#include "XmlDocDocument.hpp"
#include "XmlDocExamples.hpp"
#include "XmlDocHistory.hpp"
#include "XmlDocHistoryItem.hpp"
#include "XmlDocImageItem.hpp"
#include "XmlDocKeywordItem.hpp"
#include "XmlDocLanguageItem.hpp"
#include "XmlDocLinkItem.hpp"
#include "XmlDocModuleNameItem.hpp"
#include "XmlDocParamInput.hpp"
#include "XmlDocParamOutput.hpp"
#include "XmlDocResolveLink.hpp"
#include "XmlDocSeeAlso.hpp"
#include "XmlDocShortDescriptionItem.hpp"
#include "XmlDocSyntax.hpp"
#include "XmlDocTitleItem.hpp"
#include "XmlDocUsedFunctionItem.hpp"
#include "XmlDocumentTags.hpp"
#include "XmlHelpers.hpp"
#include "characters_encoding.hpp"
#include "i18n.hpp"
#include "StringHelpers.hpp"
#include "NelsonConfiguration.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static std::ifstream&
safegetline(std::ifstream& os, std::string& line)
{
    std::string myline;
    if (getline(os, myline)) {
        if (myline.size() && myline[myline.size() - 1] == '\r') {
            line = myline.substr(0, myline.size() - 1);
        } else {
            line = myline;
        }
    }
    return os;
}
//=============================================================================
XmlDocDocument::XmlDocDocument(const std::wstring& srcfilename, const std::wstring& sectionname,
    const std::wstring& destfilename, bool bOverwriteExistingFile, DOCUMENT_OUTPUT outputTarget)
{
    this->outputTarget = outputTarget;
    this->xmlfilename.assign(srcfilename);
    this->sectionName.assign(sectionname);
    this->errorMessage.clear();
    this->warningMessage.clear();
    this->items.clear();
    this->bReadOk = false;
    this->xmlDirectory = L"./";
    FileSystemWrapper::Path pathToSplit(srcfilename);
    if (pathToSplit.has_parent_path()) {
        this->xmlDirectory = pathToSplit.parent_path().generic_wstring();
    }
    this->filenameDestination.clear();
    if (FileSystemWrapper::Path::is_directory(destfilename)) {
        this->directoryDestination = destfilename;
        FileSystemWrapper::Path pathname(srcfilename);
        std::wstring nfilename;
        if (this->outputTarget == DOCUMENT_OUTPUT::MARKDOWN) {
            nfilename = pathname.stem().generic_wstring() + L".md";
        } else {
            nfilename = pathname.stem().generic_wstring() + L".html";
        }
        FileSystemWrapper::Path pathdest(destfilename);
        pathdest = pathdest / nfilename;
        this->filenameDestination = pathdest.generic_wstring();
    } else {
        this->directoryDestination.clear();
        this->setDestinationFile(destfilename);
    }
    this->bOverwriteExistingFile = bOverwriteExistingFile;
    this->previousLinkName.clear();
    this->previousLinkUrl.clear();
    this->nextLinkName.clear();
    this->nextLinkUrl.clear();
    this->indexLinkName.clear();
    this->indexLinkUrl.clear();
}
//=============================================================================
XmlDocDocument::XmlDocDocument(std::vector<XmlDocGenericItem*> items,
    const std::wstring& srcfilename, const std::wstring& destfilename, bool bOverwriteExistingFile,
    DOCUMENT_OUTPUT outputTarget)
{
    this->outputTarget = outputTarget;
    this->xmlfilename.assign(srcfilename);
    this->errorMessage.clear();
    this->warningMessage.clear();
    this->items = items;
    this->bReadOk = true;
    this->xmlDirectory = L"./";
    FileSystemWrapper::Path pathToSplit = srcfilename;
    if (pathToSplit.has_parent_path()) {
        this->xmlDirectory = pathToSplit.parent_path().generic_wstring();
    }
    this->filenameDestination.clear();
    if (FileSystemWrapper::Path::is_directory(destfilename)) {
        this->directoryDestination = destfilename;
        FileSystemWrapper::Path pathname(srcfilename);
        std::wstring nfilename;
        if (this->outputTarget == DOCUMENT_OUTPUT::MARKDOWN) {
            nfilename = pathname.stem().generic_wstring() + L".md";
        } else {
            nfilename = pathname.stem().generic_wstring() + L".html";
        }
        FileSystemWrapper::Path pathdest(destfilename);
        pathdest = pathdest.normalize();
        pathdest = pathdest / nfilename;
        this->filenameDestination = pathdest.generic_wstring();
    } else {
        this->directoryDestination.clear();
        FileSystemWrapper::Path pathdest(destfilename);
        pathdest = pathdest.normalize();
        this->setDestinationFile(pathdest.generic_wstring());
    }
    this->bOverwriteExistingFile = bOverwriteExistingFile;
    this->previousLinkName.clear();
    this->previousLinkUrl.clear();
    this->nextLinkName.clear();
    this->nextLinkUrl.clear();
    this->indexLinkName.clear();
    this->indexLinkUrl.clear();
}
//=============================================================================
XmlDocDocument::~XmlDocDocument()
{
    this->outputTarget = DOCUMENT_OUTPUT::HMTL;
    clearItems();
    this->xmlfilename.clear();
    this->xmlDirectory = L"./";
    this->errorMessage.clear();
    this->warningMessage.clear();
    this->filenameDestination.clear();
    this->directoryDestination.clear();
    this->bOverwriteExistingFile = false;
    this->bReadOk = false;
    this->previousLinkName.clear();
    this->previousLinkUrl.clear();
    this->nextLinkName.clear();
    this->nextLinkUrl.clear();
    this->indexLinkName.clear();
    this->indexLinkUrl.clear();
}
//=============================================================================
void
XmlDocDocument::clearItems()
{
    for (auto& item : this->items) {
        enum XMLDOC_ITEMS_ID id = stringTagToId(wstring_to_utf8(item->getItemType()));
        switch (id) {
        case XMLDOC_ITEMS_ID::LANGUAGE_TAG_ID: {
            if (item) {
                XmlDocLanguageItem* ptr = (XmlDocLanguageItem*)item;
                delete ptr;
                ptr = nullptr;
            }
        } break;
        case XMLDOC_ITEMS_ID::TITLE_TAG_ID: {
            if (item) {
                XmlDocTitleItem* ptr = (XmlDocTitleItem*)item;
                delete ptr;
                ptr = nullptr;
            }
        } break;
        case XMLDOC_ITEMS_ID::KEYWORD_TAG_ID: {
            if (item) {
                XmlDocKeywordItem* ptr = (XmlDocKeywordItem*)item;
                delete ptr;
                ptr = nullptr;
            }
        } break;
        case XMLDOC_ITEMS_ID::SHORT_DESCRIPTION_TAG_ID: {
            if (item) {
                XmlDocShortDescriptionItem* ptr = (XmlDocShortDescriptionItem*)item;
                delete ptr;
                ptr = nullptr;
            }
        } break;
        case XMLDOC_ITEMS_ID::COPYRIGHT_TAG_ID: {
            if (item) {
                XmlDocCopyrightItem* ptr = (XmlDocCopyrightItem*)item;
                delete ptr;
                ptr = nullptr;
            }
        } break;
        case XMLDOC_ITEMS_ID::SYNTAX_TAG_ID: {
            if (item) {
                XmlDocSyntax* ptr = (XmlDocSyntax*)item;
                delete ptr;
                ptr = nullptr;
            }
        } break;
        case XMLDOC_ITEMS_ID::PARAM_INPUT_TAG_ID: {
            if (item) {
                XmlDocParamInput* ptr = (XmlDocParamInput*)item;
                delete ptr;
                ptr = nullptr;
            }
        } break;
        case XMLDOC_ITEMS_ID::PARAM_OUTPUT_TAG_ID: {
            if (item) {
                XmlDocParamOutput* ptr = (XmlDocParamOutput*)item;
                delete ptr;
                ptr = nullptr;
            }
        } break;
        case XMLDOC_ITEMS_ID::DESCRIPTION_TAG_ID: {
            if (item) {
                XmlDocDescriptionItem* ptr = (XmlDocDescriptionItem*)item;
                delete ptr;
                ptr = nullptr;
            }
        } break;
        case XMLDOC_ITEMS_ID::USED_FUNCTION_TAG_ID: {
            if (item) {
                XmlDocUsedFunctionItem* ptr = (XmlDocUsedFunctionItem*)item;
                delete ptr;
                ptr = nullptr;
            }
        } break;
        case XMLDOC_ITEMS_ID::BIBLIOGRAPHY_TAG_ID: {
            if (item) {
                XmlDocBibliographyItem* ptr = (XmlDocBibliographyItem*)item;
                delete ptr;
                ptr = nullptr;
            }
        } break;
        case XMLDOC_ITEMS_ID::EXAMPLES_TAG_ID: {
            if (item) {
                XmlDocExamples* ptr = (XmlDocExamples*)item;
                delete ptr;
                ptr = nullptr;
            }
        } break;
        case XMLDOC_ITEMS_ID::SEE_ALSO_TAG_ID: {
            if (item) {
                XmlDocSeeAlso* ptr = (XmlDocSeeAlso*)item;
                delete ptr;
                ptr = nullptr;
            }
        } break;
        case XMLDOC_ITEMS_ID::HISTORY_TAG_ID: {
            if (item) {
                XmlDocHistory* ptr = (XmlDocHistory*)item;
                delete ptr;
                ptr = nullptr;
            }
        } break;
        case XMLDOC_ITEMS_ID::AUTHORS_TAG_ID: {
            if (item) {
                XmlDocAuthors* ptr = (XmlDocAuthors*)item;
                delete ptr;
                ptr = nullptr;
            }
        } break;
        case XMLDOC_ITEMS_ID::CHAPTER_TAG_ID: {
            if (item) {
                XmlDocChapterItem* ptr = (XmlDocChapterItem*)item;
                delete ptr;
                ptr = nullptr;
            }
        } break;
        case XMLDOC_ITEMS_ID::CHAPTER_DESCRIPTION_TAG_ID: {
            if (item) {
                XmlDocChapterDescriptionItem* ptr = (XmlDocChapterDescriptionItem*)item;
                delete ptr;
                ptr = nullptr;
            }
        } break;
        case XMLDOC_ITEMS_ID::CHAPTER_INDEX_TAG_ID: {
            if (item) {
                XmlDocChapterIndexItem* ptr = (XmlDocChapterIndexItem*)item;
                delete ptr;
                ptr = nullptr;
            }
        } break;
        case XMLDOC_ITEMS_ID::MODULE_NAME_TAG_ID: {
            if (item) {
                XmlDocModuleNameItem* ptr = (XmlDocModuleNameItem*)item;
                delete ptr;
                ptr = nullptr;
            }
        } break;
        case XMLDOC_ITEMS_ID::IMAGE_TAG_ID: {
            if (item) {
                XmlDocImageItem* ptr = (XmlDocImageItem*)item;
                delete ptr;
                ptr = nullptr;
            }
        } break;
        default: {
        } break;
        }
    }
    this->items.clear();
}
//=============================================================================
wstringVector
XmlDocDocument::getError()
{
    return this->errorMessage;
}
//=============================================================================
wstringVector
XmlDocDocument::getWarning()
{
    return this->warningMessage;
}
//=============================================================================
std::wstring
XmlDocDocument::getFilename()
{
    return this->xmlfilename;
}
//=============================================================================
size_t
XmlDocDocument::count(const std::string& tag)
{
    size_t nbref = 0;
    for (auto& item : this->items) {
        if (item->getItemType() == utf8_to_wstring(tag)) {
            nbref++;
        }
    }
    return nbref;
}
//=============================================================================
std::vector<XmlDocGenericItem*>
XmlDocDocument::getXmlDocGenericItems()
{
    return items;
}
//=============================================================================
bool
XmlDocDocument::readFile()
{
    if (this->bReadOk) {
        return true;
    }
#ifdef _MSC_VER
    std::ifstream xmlDocFile(this->xmlfilename);
#else
    std::ifstream xmlDocFile(wstring_to_utf8(this->xmlfilename));
#endif
    std::string xmlDocString = "";
    if (!xmlDocFile.is_open()) {
        return false;
    }
    std::string tmpline;
    while (safegetline(xmlDocFile, tmpline)) {
        xmlDocString += tmpline + '\n';
    }
    xmlDocFile.close();
    std::wstring error;
    xmlDocPtr doc = readDocument(xmlDocString, false, error);
    if (!error.empty()) {
        errorMessage.push_back(error);
        xmlFreeDoc(doc);
        return false;
    }
    if (!doc) {
        xmlFreeDoc(doc);
        return false;
    }
    xmlNodePtr currentNode = xmlDocGetRootElement(doc);
    xmlNodePtr xmlDocNode = nullptr;
    size_t nbXmlDocTag = 0;
    while (currentNode != nullptr) {
        std::string currentNodeName = std::string((char*)currentNode->name);
        if (currentNodeName != XMLDOC_TAG && currentNodeName != XML_COMMENT_TAG) {
            xmlFreeDoc(doc);
            this->errorMessage.push_back(_W("line ") + std::to_wstring(currentNode->line) + _W(": ")
                + utf8_to_wstring(currentNodeName) + L" " + _W("not managed."));
            this->bReadOk = false;
            return false;
        }
        if (currentNodeName == XMLDOC_TAG) {
            xmlDocNode = currentNode;
            nbXmlDocTag++;
        }
        currentNode = currentNode->next;
    }
    if (nbXmlDocTag == 0) {
        std::wstring line = currentNode ? std::to_wstring(currentNode->line) : L"";
        std::wstring tag = utf8_to_wstring(XMLDOC_TAG);
        std::wstring msg;
        if (!line.empty()) {
            msg.append(_W("line ") + line + _W(": "));
        }
        msg.append(tag + L" " + _W("missing."));
        this->errorMessage.push_back(msg);
        this->bReadOk = false;
        xmlFreeDoc(doc);
        return false;
    }
    if (nbXmlDocTag > 1) {
        std::wstring line = currentNode ? std::to_wstring(currentNode->line) : L"";
        std::wstring tag = utf8_to_wstring(XMLDOC_TAG);
        std::wstring msg;
        if (!line.empty()) {
            msg.append(_W("line ") + std::to_wstring(currentNode->line) + _W(": "));
        }
        msg.append(tag + L" " + _W("duplicated."));
        this->errorMessage.push_back(msg);
        this->bReadOk = false;
        xmlFreeDoc(doc);
        return false;
    }
    currentNode = xmlDocNode;
    currentNode = currentNode->xmlChildrenNode;
    while (currentNode != nullptr) {
        std::string currentNodeName = std::string((char*)currentNode->name);
        if (currentNodeName != XML_COMMENT_TAG) {
            bool res = false;
            enum XMLDOC_ITEMS_ID id = stringTagToId(currentNodeName);
            switch (id) {
            case XMLDOC_ITEMS_ID::XML_COMMENT_TAG_ID: {
                res = true;
            } break;
            case XMLDOC_ITEMS_ID::LANGUAGE_TAG_ID: {
                res = readFileCaseLanguage(doc, currentNode);
            } break;
            case XMLDOC_ITEMS_ID::TITLE_TAG_ID: {
                res = readFileCaseTitle(doc, currentNode);
            } break;
            case XMLDOC_ITEMS_ID::KEYWORD_TAG_ID: {
                res = readFileCaseKeyword(doc, currentNode);
            } break;
            case XMLDOC_ITEMS_ID::COPYRIGHT_TAG_ID: {
                res = readFileCaseCopyright(doc, currentNode);
            } break;
            case XMLDOC_ITEMS_ID::DESCRIPTION_TAG_ID: {
                res = readFileCaseDescription(doc, currentNode);
            } break;
            case XMLDOC_ITEMS_ID::SHORT_DESCRIPTION_TAG_ID: {
                res = readFileCaseShortDescription(doc, currentNode);
            } break;
            case XMLDOC_ITEMS_ID::USED_FUNCTION_TAG_ID: {
                res = readFileCaseUsedFunction(doc, currentNode);
            } break;
            case XMLDOC_ITEMS_ID::BIBLIOGRAPHY_TAG_ID: {
                res = readFileCaseBibliography(doc, currentNode);
            } break;
            case XMLDOC_ITEMS_ID::SYNTAX_TAG_ID: {
                res = readFileCaseSyntax(doc, currentNode);
            } break;
            case XMLDOC_ITEMS_ID::PARAM_INPUT_TAG_ID: {
                res = readFileCaseParamInput(doc, currentNode);
            } break;
            case XMLDOC_ITEMS_ID::PARAM_OUTPUT_TAG_ID: {
                res = readFileCaseParamOutput(doc, currentNode);
            } break;
            case XMLDOC_ITEMS_ID::EXAMPLES_TAG_ID: {
                res = readFileCaseExamples(doc, currentNode);
            } break;
            case XMLDOC_ITEMS_ID::SEE_ALSO_TAG_ID: {
                res = readFileCaseSeeAlso(doc, currentNode);
            } break;
            case XMLDOC_ITEMS_ID::HISTORY_TAG_ID: {
                res = readFileCaseHistory(doc, currentNode);
            } break;
            case XMLDOC_ITEMS_ID::AUTHORS_TAG_ID: {
                res = readFileCaseAuthors(doc, currentNode);
            } break;
            case XMLDOC_ITEMS_ID::CHAPTER_TAG_ID: {
                res = readFileCaseChapter(doc, currentNode);
            } break;
            case XMLDOC_ITEMS_ID::CHAPTER_DESCRIPTION_TAG_ID: {
                res = readFileCaseChapterDescription(doc, currentNode);
            } break;
            case XMLDOC_ITEMS_ID::IMAGE_TAG_ID: {
                res = readFileCaseImage(doc, currentNode);
            } break;
            case XMLDOC_ITEMS_ID::CHAPTER_INDEX_TAG_ID: {
                res = readFileCaseChapterIndex(doc, currentNode);
            } break;
            case XMLDOC_ITEMS_ID::MODULE_NAME_TAG_ID: {
                res = readFileCaseModuleName(doc, currentNode);
            } break;
            default: {
                xmlFreeDoc(doc);
                this->errorMessage.push_back(_W("line ") + std::to_wstring(currentNode->line)
                    + _W(": ") + utf8_to_wstring(currentNodeName) + L" " + _W("not managed."));
                this->bReadOk = false;
                return false;
            } break;
            }
            if (!res) {
                return res;
            }
        }
        currentNode = currentNode->next;
    }
    xmlFreeDoc(doc);
    this->bReadOk = true;
    return true;
}
//=============================================================================
bool
XmlDocDocument::isKeywordDocument()
{
    if (this->bReadOk) {
        return (count(KEYWORD_TAG) == 1);
    }
    return false;
}
//=============================================================================
bool
XmlDocDocument::isChapterDocument()
{
    if (this->bReadOk) {
        return (count(CHAPTER_TAG) == 1);
    }
    return false;
}
//=============================================================================
bool
XmlDocDocument::isTitleDocument()
{
    if (this->bReadOk) {
        return (count(TITLE_TAG) == 1);
    }
    return false;
}
//=============================================================================
XmlDocGenericItem*
XmlDocDocument::findfirst(const std::string& tag)
{
    if (count(tag) > 0) {
        for (auto& item : this->items) {
            if (item->getItemType() == utf8_to_wstring(tag)) {
                return item;
            }
        }
    }
    return nullptr;
}
//=============================================================================
std::wstring
XmlDocDocument::getPageTitle()
{
    if (!this->getTitle().empty()) {
        return this->getTitle();
    }
    if (!this->getKeyword().empty()) {
        return this->getKeyword();
    }
    return L"";
}
//=============================================================================
std::wstring
XmlDocDocument::getPageDescription()
{
    return this->getShortDescription();
}
//=============================================================================
std::wstring
XmlDocDocument::getShortDescription()
{
    XmlDocGenericItem* pItem = findfirst(SHORT_DESCRIPTION_TAG);
    if (pItem) {
        XmlDocShortDescriptionItem* pShortDescriptionItem = (XmlDocShortDescriptionItem*)pItem;
        return pShortDescriptionItem->getValue();
    }
    return L"";
}
//=============================================================================
std::wstring
XmlDocDocument::getLanguage()
{
    XmlDocGenericItem* pItem = findfirst(LANGUAGE_TAG);
    if (pItem) {
        XmlDocLanguageItem* pLanguageItem = (XmlDocLanguageItem*)pItem;
        return pLanguageItem->getValue();
    }
    return L"";
}
//=============================================================================
std::wstring
XmlDocDocument::getTitle()
{
    XmlDocGenericItem* pItem = findfirst(TITLE_TAG);
    if (pItem) {
        XmlDocTitleItem* pTitleItem = (XmlDocTitleItem*)pItem;
        return pTitleItem->getValue();
    }
    return L"";
}
//=============================================================================
std::wstring
XmlDocDocument::getChapter()
{
    XmlDocGenericItem* pItem = findfirst(CHAPTER_TAG);
    if (pItem) {
        XmlDocChapterItem* pChapterItem = (XmlDocChapterItem*)pItem;
        return pChapterItem->getValue();
    }
    return L"";
}
//=============================================================================
std::wstring
XmlDocDocument::getKeyword()
{
    XmlDocGenericItem* pItem = findfirst(KEYWORD_TAG);
    if (pItem) {
        XmlDocKeywordItem* pKeywordItem = (XmlDocKeywordItem*)pItem;
        return pKeywordItem->getValue();
    }
    return L"";
}
//=============================================================================
std::wstring
XmlDocDocument::getCopyright()
{
    XmlDocGenericItem* pItem = findfirst(COPYRIGHT_TAG);
    if (pItem) {
        XmlDocCopyrightItem* pCopyrightItem = (XmlDocCopyrightItem*)pItem;
        return pCopyrightItem->getValue();
    }
    return L"";
}
//=============================================================================
bool
XmlDocDocument::haveExample()
{
    return (count(EXAMPLES_TAG) > 0);
}
//=============================================================================
enum XMLDOC_ITEMS_ID
XmlDocDocument::stringTagToId(const std::string& tag)
{
    if (tag == XML_COMMENT_TAG) {
        return XMLDOC_ITEMS_ID::XML_COMMENT_TAG_ID;
    }
    if (tag == XMLDOC_TAG) {
        return XMLDOC_ITEMS_ID::XMLDOC_TAG_ID;
    }
    if (tag == LANGUAGE_TAG) {
        return XMLDOC_ITEMS_ID::LANGUAGE_TAG_ID;
    }
    if (tag == TITLE_TAG) {
        return XMLDOC_ITEMS_ID::TITLE_TAG_ID;
    }
    if (tag == KEYWORD_TAG) {
        return XMLDOC_ITEMS_ID::KEYWORD_TAG_ID;
    }
    if (tag == SHORT_DESCRIPTION_TAG) {
        return XMLDOC_ITEMS_ID::SHORT_DESCRIPTION_TAG_ID;
    }
    if (tag == COPYRIGHT_TAG) {
        return XMLDOC_ITEMS_ID::COPYRIGHT_TAG_ID;
    }
    if (tag == SYNTAX_TAG) {
        return XMLDOC_ITEMS_ID::SYNTAX_TAG_ID;
    }
    if (tag == PARAM_INPUT_TAG) {
        return XMLDOC_ITEMS_ID::PARAM_INPUT_TAG_ID;
    }
    if (tag == PARAM_OUTPUT_TAG) {
        return XMLDOC_ITEMS_ID::PARAM_OUTPUT_TAG_ID;
    }
    if (tag == DESCRIPTION_TAG) {
        return XMLDOC_ITEMS_ID::DESCRIPTION_TAG_ID;
    }
    if (tag == USED_FUNCTION_TAG) {
        return XMLDOC_ITEMS_ID::USED_FUNCTION_TAG_ID;
    }
    if (tag == BIBLIOGRAPHY_TAG) {
        return XMLDOC_ITEMS_ID::BIBLIOGRAPHY_TAG_ID;
    }
    if (tag == EXAMPLES_TAG) {
        return XMLDOC_ITEMS_ID::EXAMPLES_TAG_ID;
    }
    if (tag == SEE_ALSO_TAG) {
        return XMLDOC_ITEMS_ID::SEE_ALSO_TAG_ID;
    }
    if (tag == HISTORY_TAG) {
        return XMLDOC_ITEMS_ID::HISTORY_TAG_ID;
    }
    if (tag == AUTHORS_TAG) {
        return XMLDOC_ITEMS_ID::AUTHORS_TAG_ID;
    }
    if (tag == CHAPTER_TAG) {
        return XMLDOC_ITEMS_ID::CHAPTER_TAG_ID;
    }
    if (tag == IMAGE_TAG) {
        return XMLDOC_ITEMS_ID::IMAGE_TAG_ID;
    }
    if (tag == CHAPTER_DESCRIPTION_TAG) {
        return XMLDOC_ITEMS_ID::CHAPTER_DESCRIPTION_TAG_ID;
    }
    if (tag == CHAPTER_INDEX_TAG) {
        return XMLDOC_ITEMS_ID::CHAPTER_INDEX_TAG_ID;
    }
    if (tag == MODULE_NAME_TAG) {
        return XMLDOC_ITEMS_ID::MODULE_NAME_TAG_ID;
    }
    return XMLDOC_ITEMS_ID::ERROR_ID;
}
//=============================================================================
void
XmlDocDocument::writeNavigatorBarAsHtml(std::string& utf8stream)
{
    if (!this->indexLinkName.empty() || !this->previousLinkName.empty()
        || !this->nextLinkName.empty()) {
        utf8stream = utf8stream + "<div class = \"manualnavbar\">" + "\n";
        utf8stream = utf8stream + "<table width = \"100%\"><tr>" + "\n";
        utf8stream = utf8stream + "\n";
        if (!this->previousLinkName.empty()) {
            utf8stream = utf8stream + R"(<td width="30%" class="previous">)" + "\n";
            utf8stream = utf8stream + R"(<span class="top"><a href=")"
                + wstring_to_utf8(this->previousLinkUrl) + "\">" + "&lt;&lt; "
                + wstring_to_utf8(this->previousLinkName) + "</a></span>" + "\n";
            utf8stream = utf8stream + "</td>" + "\n";
            utf8stream = utf8stream + "\n";
        }
        if (!this->indexLinkName.empty()) {
            utf8stream = utf8stream + R"(<td width="40%" class="center">)" + "\n";
            utf8stream = utf8stream + R"(<span class="top"><a href=")"
                + wstring_to_utf8(this->indexLinkUrl) + "\">" + wstring_to_utf8(this->indexLinkName)
                + "</a></span>" + "\n";
            utf8stream = utf8stream + "</td>" + "\n";
            utf8stream = utf8stream + "\n";
        }
        if (!this->nextLinkName.empty()) {
            utf8stream = utf8stream + R"(<td width = "30%" class = "next">)" + "\n";
            utf8stream = utf8stream + R"(<span class = "next"><a href = ")"
                + wstring_to_utf8(this->nextLinkUrl) + "\">" + wstring_to_utf8(this->nextLinkName)
                + " &gt;&gt;</a></span>" + "\n";
            utf8stream = utf8stream + "</td>" + "\n";
            utf8stream = utf8stream + "\n";
        }
        utf8stream = utf8stream + "</tr></table>" + "\n";
        utf8stream = utf8stream + "<hr />" + "\n";
        utf8stream = utf8stream + "\n";
    }
}
//=============================================================================
bool
XmlDocDocument::writeAsMarkdown(std::string& utf8stream)
{
    utf8stream = utf8stream + "\n";
    // header for examples
    if (haveExample()) {
        XmlDocGenericItem* pItem = findfirst(EXAMPLES_TAG);
        XmlDocExamples* pExamples = (XmlDocExamples*)pItem;
        pExamples->writeHeaderAsMarkdown(utf8stream);
    }
    // header
    if (isKeywordDocument()) {
        XmlDocGenericItem* pItem = findfirst(KEYWORD_TAG);
        if (pItem) {
            XmlDocKeywordItem* pItemKeyword = (XmlDocKeywordItem*)pItem;
            pItemKeyword->writeHeaderAsMarkdown(utf8stream);
        }
    }
    if (isChapterDocument()) {
        XmlDocGenericItem* pItem = findfirst(CHAPTER_TAG);
        if (pItem) {
            XmlDocChapterItem* pItemChapter = (XmlDocChapterItem*)pItem;
            pItemChapter->writeHeaderAsMarkdown(utf8stream);
        }
    }
    if (isTitleDocument()) {
        XmlDocGenericItem* pItem = findfirst(TITLE_TAG);
        XmlDocTitleItem* pItemTitle = (XmlDocTitleItem*)pItem;
        pItemTitle->writeHeaderAsMarkdown(utf8stream); //-V522
    }
    utf8stream = utf8stream + "\n";
    for (auto& item : this->items) {
        std::wstring currentItemType = item->getItemType();
        if ((currentItemType != utf8_to_wstring(LANGUAGE_TAG))
            && (currentItemType != utf8_to_wstring(COPYRIGHT_TAG))) {
            item->writeAsMarkdown(utf8stream);
        }
    }
    utf8stream = utf8stream + "\n";
    return true;
}
//=============================================================================
bool
XmlDocDocument::writeAsHtml(std::string& utf8stream)
{
    utf8stream = utf8stream + HTML_DOCTYPE_HTML_TAG + "\n";
    utf8stream = utf8stream + "\n";
    utf8stream = utf8stream + HTML_HTML_IN_TAG + "\n";
    if (!getCopyright().empty()) {
        XmlDocGenericItem* pItem = findfirst(COPYRIGHT_TAG);
        XmlDocCopyrightItem* pCopyrightItem = (XmlDocCopyrightItem*)pItem;
        pCopyrightItem->writeAsHtml(utf8stream); //-V522
    }
    utf8stream = utf8stream + HTML_HEAD_IN_TAG + "\n";
    utf8stream = utf8stream + HTML_GENERATOR_TAG + "\n";
    // default style
    utf8stream
        = utf8stream + R"(<link rel="stylesheet" type="text/css" href="style.css" />)" + "\n";
    // header for examples
    if (haveExample()) {
        XmlDocGenericItem* pItem = findfirst(EXAMPLES_TAG);
        XmlDocExamples* pExamples = (XmlDocExamples*)pItem;
        pExamples->writeHeaderAsHtml(utf8stream); //-V522
    }
    // header
    if (isKeywordDocument()) {
        XmlDocGenericItem* pItem = findfirst(KEYWORD_TAG);
        if (pItem) {
            XmlDocKeywordItem* pItemKeyword = (XmlDocKeywordItem*)pItem;
            pItemKeyword->writeHeaderAsHtml(utf8stream);
        }
    }
    if (isChapterDocument()) {
        XmlDocGenericItem* pItem = findfirst(CHAPTER_TAG);
        if (pItem) {
            XmlDocChapterItem* pItemChapter = (XmlDocChapterItem*)pItem;
            pItemChapter->writeHeaderAsHtml(utf8stream);
        }
    }
    if (isTitleDocument()) {
        XmlDocGenericItem* pItem = findfirst(TITLE_TAG);
        XmlDocTitleItem* pItemTitle = (XmlDocTitleItem*)pItem;
        pItemTitle->writeHeaderAsHtml(utf8stream); //-V522
    }
    utf8stream = utf8stream + HTML_HEAD_OUT_TAG + "\n";
    utf8stream = utf8stream + HTML_BODY_IN_TAG + "\n";
    utf8stream = utf8stream + "\n";
    this->writeNavigatorBarAsHtml(utf8stream);
    for (auto& item : this->items) {
        std::wstring currentItemType = item->getItemType();
        if ((currentItemType != utf8_to_wstring(LANGUAGE_TAG))
            && (currentItemType != utf8_to_wstring(COPYRIGHT_TAG))) {
            item->writeAsHtml(utf8stream);
        }
    }
    this->writeNavigatorBarAsHtml(utf8stream);
    utf8stream = utf8stream + HTML_BODY_OUT_TAG + "\n";
    utf8stream = utf8stream + "\n";
    utf8stream = utf8stream + HTML_HTML_OUT_TAG + "\n";
    return true;
}
//=============================================================================
bool
XmlDocDocument::readFileCaseUsedFunction(xmlDocPtr doc, xmlNodePtr node)
{
    if (node->content) {
        xmlFreeDoc(doc);
        errorMessage.push_back(_W("line ") + std::to_wstring(node->line) + _W(": ")
            + utf8_to_wstring(USED_FUNCTION_TAG) + L" " + _W("invalid."));
        this->bReadOk = false;
        return false;
    }
    if (node->children == nullptr) {
        warningMessage.push_back(_W("line ") + std::to_wstring(node->line) + _W(": ")
            + utf8_to_wstring(USED_FUNCTION_TAG) + L" " + _W("is empty."));
    } else {
        xmlNodePtr usedFunctionNode = node->children;
        if (usedFunctionNode->children) {
            xmlFreeDoc(doc);
            errorMessage.push_back(_W("line ") + std::to_wstring(usedFunctionNode->line) + _W(": ")
                + utf8_to_wstring(USED_FUNCTION_TAG) + L" " + _W("has child."));
            this->bReadOk = false;
            return false;
        }
        std::string str;
        if (usedFunctionNode->content) {
            str = std::string((char*)usedFunctionNode->content);
        }
        StringHelpers::trim(str);
        if (str.empty()) {
            warningMessage.push_back(_W("line ") + std::to_wstring(usedFunctionNode->line)
                + _W(": ") + utf8_to_wstring(USED_FUNCTION_TAG) + L" " + _W("is empty."));
        } else {
            items.push_back(new XmlDocUsedFunctionItem(utf8_to_wstring(str)));
        }
    }
    return true;
}
//=============================================================================
bool
XmlDocDocument::readFileCaseSyntax(xmlDocPtr doc, xmlNodePtr node)
{
    XmlDocSyntax* syntaxItems = nullptr;
    if (node->children == nullptr) {
        xmlFreeDoc(doc);
        errorMessage.push_back(_W("line ") + std::to_wstring(node->line) + _W(": ")
            + utf8_to_wstring(SYNTAX_TAG) + L" " + _W("has no child."));
        this->bReadOk = false;
        return false;
    }
    try {
        syntaxItems = new XmlDocSyntax();
    } catch (const std::bad_alloc&) {
        xmlFreeDoc(doc);
        errorMessage.push_back(_W("cannot allocate memory."));
        this->bReadOk = false;
        return false;
    }
    xmlNodePtr syntaxItemNode = node->children;
    while (syntaxItemNode != nullptr) {
        std::string currentNodeName = std::string((char*)syntaxItemNode->name);
        if (currentNodeName == XML_COMMENT_TAG) {
            // NOTHING TO DO :)
        } else if (currentNodeName == SYNTAX_ITEM_TAG) {
            if (syntaxItemNode->content) {
                if (syntaxItems) {
                    delete syntaxItems;
                    syntaxItems = nullptr;
                }
                xmlFreeDoc(doc);
                errorMessage.push_back(_W("line ") + std::to_wstring(syntaxItemNode->line)
                    + _W(": ") + utf8_to_wstring(SYNTAX_ITEM_TAG) + L" " + _W("invalid."));
                this->bReadOk = false;
                return false;
            }
            xmlNodePtr itemNode = syntaxItemNode->children;
            if (itemNode->children) {
                if (syntaxItems) {
                    delete syntaxItems;
                    syntaxItems = nullptr;
                }
                xmlFreeDoc(doc);
                errorMessage.push_back(_W("line ") + std::to_wstring(itemNode->line) + _W(": ")
                    + utf8_to_wstring(SYNTAX_ITEM_TAG) + L" " + _W("has child."));
                this->bReadOk = false;
                return false;
            }
            std::string str;
            if (itemNode->content) {
                str = std::string((char*)itemNode->content);
            }
            StringHelpers::trim(str);
            if (str.empty()) {
                if (syntaxItems) {
                    delete syntaxItems;
                    syntaxItems = nullptr;
                }
                xmlFreeDoc(doc);
                errorMessage.push_back(_W("line ") + std::to_wstring(itemNode->line) + _W(": ")
                    + utf8_to_wstring(SYNTAX_ITEM_TAG) + L" " + _W("is empty."));
                this->bReadOk = false;
                return false;
            }
            if (syntaxItems) {
                syntaxItems->append(utf8_to_wstring(str));
            }

        } else {
            xmlFreeDoc(doc);
            this->errorMessage.push_back(_W("line ") + std::to_wstring(syntaxItemNode->line)
                + _W(": ") + utf8_to_wstring(currentNodeName) + L" " + _W("not managed."));
            this->bReadOk = false;
            return false;
        }
        syntaxItemNode = syntaxItemNode->next;
    }
    items.push_back(syntaxItems);
    return true;
}
//=============================================================================
bool
XmlDocDocument::readFileCaseLanguage(xmlDocPtr doc, xmlNodePtr node)
{
    if (node->content) {
        xmlFreeDoc(doc);
        errorMessage.push_back(_W("line ") + std::to_wstring(node->line) + _W(": ")
            + utf8_to_wstring(LANGUAGE_TAG) + L" " + _W("invalid."));
        this->bReadOk = false;
        return false;
    }
    xmlNodePtr languageNode = node->children;
    if (languageNode->children) {
        xmlFreeDoc(doc);
        errorMessage.push_back(_W("line ") + std::to_wstring(languageNode->line) + _W(": ")
            + utf8_to_wstring(LANGUAGE_TAG) + L" " + _W("has child."));
        this->bReadOk = false;
        return false;
    }
    std::string str;
    if (languageNode->content) {
        str = std::string((char*)languageNode->content);
    }
    StringHelpers::trim(str);
    if (str.empty()) {
        xmlFreeDoc(doc);
        errorMessage.push_back(_W("line ") + std::to_wstring(languageNode->line) + _W(": ")
            + utf8_to_wstring(LANGUAGE_TAG) + L" " + _W("is empty."));
        this->bReadOk = false;
        return false;
    }
    std::wstring wlang = utf8_to_wstring(str);
    if (!Localization::Instance()->isSupportedLanguage(wlang)) {
        xmlFreeDoc(doc);
        errorMessage.push_back(_W("line ") + std::to_wstring(languageNode->line) + _W(": ")
            + utf8_to_wstring(LANGUAGE_TAG) + L" " + wlang + _W("not supported."));
        this->bReadOk = false;
        return false;
    }
    items.push_back(new XmlDocLanguageItem(wlang));
    return true;
}
//=============================================================================
bool
XmlDocDocument::readFileCaseTitle(xmlDocPtr doc, xmlNodePtr node)
{
    if (node->content) {
        xmlFreeDoc(doc);
        errorMessage.push_back(_W("line ") + std::to_wstring(node->line) + _W(": ")
            + utf8_to_wstring(TITLE_TAG) + L" " + _W("invalid."));
        this->bReadOk = false;
        return false;
    }
    xmlNodePtr titleNode = node->children;
    if (titleNode->children) {
        xmlFreeDoc(doc);
        errorMessage.push_back(_W("line ") + std::to_wstring(titleNode->line) + _W(": ")
            + utf8_to_wstring(TITLE_TAG) + L" " + _W("has child."));
        this->bReadOk = false;
        return false;
    }
    std::string str;
    if (titleNode->content) {
        str = std::string((char*)titleNode->content);
    }
    StringHelpers::trim(str);
    if (str.empty()) {
        xmlFreeDoc(doc);
        errorMessage.push_back(_W("line ") + std::to_wstring(titleNode->line) + _W(": ")
            + utf8_to_wstring(TITLE_TAG) + L" " + _W("is empty."));
        this->bReadOk = false;
        return false;
    }
    items.push_back(new XmlDocTitleItem(utf8_to_wstring(str)));
    return true;
}
//=============================================================================
bool
XmlDocDocument::readFileCaseKeyword(xmlDocPtr doc, xmlNodePtr node)
{
    if (node->content) {
        xmlFreeDoc(doc);
        errorMessage.push_back(_W("line ") + std::to_wstring(node->line) + _W(": ")
            + utf8_to_wstring(KEYWORD_TAG) + L" " + _W("invalid."));
        this->bReadOk = false;
        return false;
    }
    xmlNodePtr keywordNode = node->children;
    if (keywordNode->children) {
        xmlFreeDoc(doc);
        errorMessage.push_back(_W("line ") + std::to_wstring(keywordNode->line) + _W(": ")
            + utf8_to_wstring(KEYWORD_TAG) + L" " + _W("has child."));
        this->bReadOk = false;
        return false;
    }
    std::string str;
    if (keywordNode->content) {
        str = std::string((char*)keywordNode->content);
    }
    StringHelpers::trim(str);
    if (str.empty()) {
        xmlFreeDoc(doc);
        errorMessage.push_back(_W("line ") + std::to_wstring(keywordNode->line) + _W(": ")
            + utf8_to_wstring(KEYWORD_TAG) + L" " + _W("is empty."));
        this->bReadOk = false;
        return false;
    }
    items.push_back(new XmlDocKeywordItem(utf8_to_wstring(str)));
    return true;
}
//=============================================================================
bool
XmlDocDocument::readFileCaseCopyright(xmlDocPtr doc, xmlNodePtr node)
{
    if (node->content) {
        xmlFreeDoc(doc);
        errorMessage.push_back(_W("line ") + std::to_wstring(node->line) + _W(": ")
            + utf8_to_wstring(COPYRIGHT_TAG) + L" " + _W("invalid."));
        this->bReadOk = false;
        return false;
    }
    xmlNodePtr copyrightNode = node->children;
    if (copyrightNode && copyrightNode->children) {
        xmlFreeDoc(doc);
        errorMessage.push_back(_W("line ") + std::to_wstring(copyrightNode->line) + _W(": ")
            + utf8_to_wstring(COPYRIGHT_TAG) + L" " + _W("has child."));
        this->bReadOk = false;
        return false;
    }
    std::string str;
    if (copyrightNode && copyrightNode->content) {
        str = std::string((char*)copyrightNode->content);
    }
    StringHelpers::trim(str);
    if (str.empty()) {
        if (copyrightNode) {
            warningMessage.push_back(_W("line ") + std::to_wstring(copyrightNode->line) + _W(": ")
                + utf8_to_wstring(COPYRIGHT_TAG) + L" " + _W("is empty."));
        } else {
            warningMessage.push_back(
                _W("line ") + _W(": ") + utf8_to_wstring(COPYRIGHT_TAG) + L" " + _W("is empty."));
        }
    } else {
        items.push_back(new XmlDocCopyrightItem(utf8_to_wstring(str)));
    }
    return true;
}
//=============================================================================
bool
XmlDocDocument::readFileCaseDescription(xmlDocPtr doc, xmlNodePtr node)
{
    xmlBufferPtr buffer = xmlBufferCreate();
    xmlNodePtr descriptionNode = node;
    xmlNodeDump(buffer, doc, descriptionNode, 0, 1);
    std::string str = std::string((char*)buffer->content);
    xmlBufferFree(buffer);
    StringHelpers::trim(str);
    if (str.empty()) {
        xmlFreeDoc(doc);
        errorMessage.push_back(_W("line ") + std::to_wstring(node->line) + _W(": ")
            + utf8_to_wstring(DESCRIPTION_TAG) + L" " + _W("is empty."));
        this->bReadOk = false;
        return false;
    }
    StringHelpers::replace_first(str, "<" + std::string(DESCRIPTION_TAG) + ">", "");
    StringHelpers::replace_last(str, "</" + std::string(DESCRIPTION_TAG) + ">", "");
    XmlDocDescriptionItem* ptrDescription = new XmlDocDescriptionItem(utf8_to_wstring(str));
    ptrDescription->setDirectories(this->xmlDirectory, this->directoryDestination);
    try {
        ptrDescription->searchImageTag();
    } catch (Exception& e) {
        errorMessage.push_back(utf8_to_wstring(DESCRIPTION_TAG) + L": " + e.getMessage());
        this->bReadOk = false;
        delete ptrDescription;
        return false;
    }
    items.push_back(ptrDescription);
    return true;
}
//=============================================================================
bool
XmlDocDocument::readFileCaseShortDescription(xmlDocPtr doc, xmlNodePtr node)
{
    if (node->content) {
        xmlFreeDoc(doc);
        errorMessage.push_back(_W("line ") + std::to_wstring(node->line) + _W(": ")
            + utf8_to_wstring(SHORT_DESCRIPTION_TAG) + L" " + _W("invalid."));
        this->bReadOk = false;
        return false;
    }
    xmlNodePtr shortDescriptionNode = node->children;
    if (shortDescriptionNode->children) {
        xmlFreeDoc(doc);
        errorMessage.push_back(_W("line ") + std::to_wstring(shortDescriptionNode->line) + _W(": ")
            + utf8_to_wstring(SHORT_DESCRIPTION_TAG) + L" " + _W("has child."));
        this->bReadOk = false;
        return false;
    }
    std::string str;
    if (shortDescriptionNode->content) {
        str = std::string((char*)shortDescriptionNode->content);
    }
    StringHelpers::trim(str);
    if (str.empty()) {
        xmlFreeDoc(doc);
        errorMessage.push_back(_W("line ") + std::to_wstring(shortDescriptionNode->line) + _W(": ")
            + utf8_to_wstring(SHORT_DESCRIPTION_TAG) + L" " + _W("is empty."));
        this->bReadOk = false;
        return false;
    }
    items.push_back(new XmlDocShortDescriptionItem(utf8_to_wstring(str)));
    return true;
}
//=============================================================================
bool
XmlDocDocument::readFileCaseBibliography(xmlDocPtr doc, xmlNodePtr node)
{
    if (node->content) {
        xmlFreeDoc(doc);
        errorMessage.push_back(_W("line ") + std::to_wstring(node->line) + _W(": ")
            + utf8_to_wstring(BIBLIOGRAPHY_TAG) + L" " + _W("invalid."));
        this->bReadOk = false;
        return false;
    }
    if (node->children == nullptr) {
        warningMessage.push_back(_W("line ") + std::to_wstring(node->line) + _W(": ")
            + utf8_to_wstring(BIBLIOGRAPHY_TAG) + L" " + _W("is empty."));
    } else {
        xmlNodePtr bibliogragphyNode = node->children;
        if (bibliogragphyNode->children) {
            xmlFreeDoc(doc);
            errorMessage.push_back(_W("line ") + std::to_wstring(bibliogragphyNode->line) + _W(": ")
                + utf8_to_wstring(BIBLIOGRAPHY_TAG) + L" " + _W("has child."));
            this->bReadOk = false;
            return false;
        }
        std::string str;
        if (bibliogragphyNode->content) {
            str = std::string((char*)bibliogragphyNode->content);
        }
        StringHelpers::trim(str);
        if (str.empty()) {
            warningMessage.push_back(_W("line ") + std::to_wstring(bibliogragphyNode->line)
                + _W(": ") + utf8_to_wstring(BIBLIOGRAPHY_TAG) + L" " + _W("is empty."));
        } else {
            items.push_back(new XmlDocBibliographyItem(utf8_to_wstring(str)));
        }
    }
    return true;
}
//=============================================================================
bool
XmlDocDocument::readFileCaseAuthors(xmlDocPtr doc, xmlNodePtr node)
{
    XmlDocAuthors* authorsItems = nullptr;
    if (node->children == nullptr) {
        xmlFreeDoc(doc);
        errorMessage.push_back(_W("line ") + std::to_wstring(node->line) + _W(": ")
            + utf8_to_wstring(AUTHORS_TAG) + L" " + _W("has no child."));
        this->bReadOk = false;
        return false;
    }
    try {
        authorsItems = new XmlDocAuthors();
    } catch (const std::bad_alloc&) {
        xmlFreeDoc(doc);
        errorMessage.push_back(_W("cannot allocate memory."));
        this->bReadOk = false;
        return false;
    }
    xmlNodePtr authorItemNode = node->children;
    while (authorItemNode != nullptr) {
        std::string currentNodeName = std::string((char*)authorItemNode->name);
        if (currentNodeName == XML_COMMENT_TAG) {
            // NOTHING TO DO :)
        } else if (currentNodeName == AUTHOR_ITEM_TAG) {
            if (authorItemNode->content) {
                if (authorsItems) {
                    delete authorsItems;
                    authorsItems = nullptr;
                }
                xmlFreeDoc(doc);
                errorMessage.push_back(_W("line ") + std::to_wstring(authorItemNode->line)
                    + _W(": ") + utf8_to_wstring(AUTHOR_ITEM_TAG) + L" " + _W("invalid."));
                this->bReadOk = false;
                return false;
            }
            xmlNodePtr itemNode = authorItemNode->children;
            if (itemNode->children) {
                if (authorsItems) {
                    delete authorsItems;
                    authorsItems = nullptr;
                }
                xmlFreeDoc(doc);
                errorMessage.push_back(_W("line ") + std::to_wstring(itemNode->line) + _W(": ")
                    + utf8_to_wstring(AUTHOR_ITEM_TAG) + L" " + _W("has child."));
                this->bReadOk = false;
                return false;
            }
            std::string str;
            if (itemNode->content) {
                str = std::string((char*)itemNode->content);
            }
            StringHelpers::trim(str);
            if (str.empty()) {
                if (authorsItems) {
                    delete authorsItems;
                    authorsItems = nullptr;
                }
                xmlFreeDoc(doc);
                errorMessage.push_back(_W("line ") + std::to_wstring(itemNode->line) + _W(": ")
                    + utf8_to_wstring(AUTHOR_ITEM_TAG) + L" " + _W("is empty."));
                this->bReadOk = false;
                return false;
            }
            if (authorsItems) {
                authorsItems->append(utf8_to_wstring(str));
            }

        } else {
            xmlFreeDoc(doc);
            this->errorMessage.push_back(_W("line ") + std::to_wstring(authorItemNode->line)
                + _W(": ") + utf8_to_wstring(currentNodeName) + L" " + _W("not managed."));
            this->bReadOk = false;
            return false;
        }
        authorItemNode = authorItemNode->next;
    }
    items.push_back(authorsItems);
    return true;
}
//=============================================================================
bool
XmlDocDocument::readFileCaseHistory(xmlDocPtr doc, xmlNodePtr node)
{
    XmlDocHistory* historyItems = nullptr;
    if (node->children == nullptr) {
        xmlFreeDoc(doc);
        errorMessage.push_back(_W("line ") + std::to_wstring(node->line) + _W(": ")
            + utf8_to_wstring(HISTORY_TAG) + L" " + _W("has no child."));
        this->bReadOk = false;
        return false;
    }
    try {
        historyItems = new XmlDocHistory();
    } catch (const std::bad_alloc&) {
        xmlFreeDoc(doc);
        errorMessage.push_back(_W("cannot allocate memory."));
        this->bReadOk = false;
        return false;
    }
    xmlNodePtr historyItemNode = node->children;
    while (historyItemNode != nullptr) {
        std::string currentNodeName = std::string((char*)historyItemNode->name);
        if (currentNodeName == XML_COMMENT_TAG) {
            // NOTHING TO DO
        } else if (currentNodeName != HISTORY_ITEM_TAG) {
            xmlFreeDoc(doc);
            this->errorMessage.push_back(_W("line ") + std::to_wstring(historyItemNode->line)
                + _W(": ") + utf8_to_wstring(currentNodeName) + L" " + _W("not managed."));
            this->bReadOk = false;
            return false;
        } else {
            std::wstring version = L"";
            std::wstring description = L"";
            int nbversiontag = 0;
            int nbdescriptiontag = 0;
            xmlNodePtr currentItemNode = historyItemNode->children;
            while (currentItemNode != nullptr) {
                std::string currentItemNodeName = std::string((char*)currentItemNode->name);
                if (currentItemNodeName == XML_COMMENT_TAG) {
                    // NOTHING TO DO :)
                } else if (currentItemNodeName == HISTORY_ITEM_VERSION_TAG) {
                    xmlNodePtr versionNode = currentItemNode;
                    versionNode = versionNode->children; //-V547
                    if (versionNode) {
                        std::string str;
                        if (versionNode->content) {
                            str = std::string((char*)versionNode->content);
                        }
                        StringHelpers::trim(str);
                        if (str.empty()) {
                            if (historyItems) {
                                delete historyItems;
                                historyItems = nullptr;
                            }
                            xmlFreeDoc(doc);
                            errorMessage.push_back(_W("line ") + std::to_wstring(versionNode->line)
                                + _W(": ") + utf8_to_wstring(HISTORY_ITEM_VERSION_TAG) + L" "
                                + _W("is empty."));
                            this->bReadOk = false;
                            return false;
                        }
                        version = utf8_to_wstring(str);
                        if (nbversiontag == 0) {
                            nbversiontag++;
                        } else {
                            if (historyItems) {
                                delete historyItems;
                                historyItems = nullptr;
                            }
                            xmlFreeDoc(doc);
                            errorMessage.push_back(_W("line ") + std::to_wstring(versionNode->line)
                                + _W(": ") + utf8_to_wstring(HISTORY_ITEM_VERSION_TAG) + L" "
                                + _W("duplicated."));
                            this->bReadOk = false;
                            return false;
                        }
                    }
                } else if (currentItemNodeName == HISTORY_ITEM_DESCRIPTION_TAG) {
                    xmlNodePtr descriptionNode = currentItemNode;
                    if (descriptionNode != nullptr) { //-V547
                        descriptionNode = descriptionNode->children;
                    }
                    if (descriptionNode) {
                        std::string str;
                        if (descriptionNode->content) {
                            str = std::string((char*)descriptionNode->content);
                        }
                        StringHelpers::trim(str);
                        if (str.empty()) {
                            if (historyItems) {
                                delete historyItems;
                                historyItems = nullptr;
                            }
                            xmlFreeDoc(doc);
                            errorMessage.push_back(_W("line ")
                                + std::to_wstring(descriptionNode->line) + _W(": ")
                                + utf8_to_wstring(HISTORY_ITEM_DESCRIPTION_TAG) + L" "
                                + _W("is empty."));
                            this->bReadOk = false;
                            return false;
                        }
                        description = utf8_to_wstring(str);
                        if (nbdescriptiontag == 0) {
                            nbdescriptiontag++;
                        } else {
                            if (historyItems) {
                                delete historyItems;
                                historyItems = nullptr;
                            }
                            xmlFreeDoc(doc);
                            errorMessage.push_back(_W("line ")
                                + std::to_wstring(descriptionNode->line) + _W(": ")
                                + utf8_to_wstring(HISTORY_ITEM_VERSION_TAG) + L" "
                                + _W("duplicated."));
                            this->bReadOk = false;
                            return false;
                        }
                    }
                } else {
                    xmlFreeDoc(doc);
                    this->errorMessage.push_back(_W("line ")
                        + std::to_wstring(currentItemNode->line) + _W(": ")
                        + utf8_to_wstring(currentItemNodeName) + L" " + _W("not managed."));
                    this->bReadOk = false;
                    return false;
                }
                currentItemNode = currentItemNode->next;
            }
            if (nbdescriptiontag == 0) {
                if (historyItems) {
                    delete historyItems;
                    historyItems = nullptr;
                }
                xmlFreeDoc(doc);
                if (currentItemNode) { //-V547
                    this->errorMessage.push_back(_W("line ")
                        + std::to_wstring(currentItemNode->line) + _W(": ")
                        + utf8_to_wstring(HISTORY_ITEM_DESCRIPTION_TAG) + L" " + _W("missing."));
                } else {
                    this->errorMessage.push_back(
                        utf8_to_wstring(HISTORY_ITEM_DESCRIPTION_TAG) + L" " + _W("missing."));
                }
                this->bReadOk = false;
                return false;
            }
            if (nbversiontag == 0) {
                if (historyItems) {
                    delete historyItems;
                    historyItems = nullptr;
                }
                xmlFreeDoc(doc);
                std::wstring message;
                if (currentItemNode == nullptr) {
                    message = utf8_to_wstring(HISTORY_ITEM_VERSION_TAG) + L" " + _W("missing.");
                } else {
                    message = _W("line ") + std::to_wstring(currentItemNode->line) //-V522
                        + _W(": ") + utf8_to_wstring(HISTORY_ITEM_VERSION_TAG) + L" "
                        + _W("missing.");
                }
                this->errorMessage.push_back(message);
                this->bReadOk = false;
                return false;
            }
            historyItems->append(version, description);
        }
        historyItemNode = historyItemNode->next;
    }
    items.push_back(historyItems);
    return true;
}
//=============================================================================
bool
XmlDocDocument::readFileCaseParamInput(xmlDocPtr doc, xmlNodePtr node)
{
    XmlDocParamInput* paramInputItems = nullptr;
    if (node->children == nullptr) {
        xmlFreeDoc(doc);
        errorMessage.push_back(_W("line ") + std::to_wstring(node->line) + _W(": ")
            + utf8_to_wstring(PARAM_INPUT_TAG) + L" " + _W("has no child."));
        this->bReadOk = false;
        return false;
    }
    try {
        paramInputItems = new XmlDocParamInput();
    } catch (const std::bad_alloc&) {
        xmlFreeDoc(doc);
        errorMessage.push_back(_W("cannot allocate memory."));
        this->bReadOk = false;
        return false;
    }
    xmlNodePtr paramInputItemNode = node->children;
    while (paramInputItemNode != nullptr) {
        std::string currentNodeName = std::string((char*)paramInputItemNode->name);
        if (currentNodeName == XML_COMMENT_TAG) {
            // NOTHING TO DO
        } else if (currentNodeName != PARAM_INPUT_ITEM_TAG) {
            xmlFreeDoc(doc);
            this->errorMessage.push_back(_W("line ") + std::to_wstring(paramInputItemNode->line)
                + _W(": ") + utf8_to_wstring(currentNodeName) + L" " + _W("not managed."));
            this->bReadOk = false;
            return false;
        } else {
            std::wstring name = L"";
            std::wstring description = L"";
            int nbnametag = 0;
            int nbdescriptiontag = 0;
            xmlNodePtr currentItemNode = paramInputItemNode->children;
            while (currentItemNode != nullptr) {
                std::string currentItemNodeName = std::string((char*)currentItemNode->name);
                if (currentItemNodeName == XML_COMMENT_TAG) {
                    // NOTHING TO DO :)
                } else if (currentItemNodeName == PARAM_NAME_TAG) {
                    xmlNodePtr nameNode = currentItemNode;
                    if (nameNode != nullptr) { //-V547
                        nameNode = nameNode->children;
                    }
                    if (nameNode) {
                        std::string str;
                        if (nameNode->content) {
                            str = std::string((char*)nameNode->content);
                        }
                        StringHelpers::trim(str);
                        if (str.empty()) {
                            if (paramInputItems) {
                                delete paramInputItems;
                                paramInputItems = nullptr;
                            }
                            xmlFreeDoc(doc);
                            errorMessage.push_back(_W("line ") + std::to_wstring(nameNode->line)
                                + _W(": ") + utf8_to_wstring(PARAM_NAME_TAG) + L" "
                                + _W("is empty."));
                            this->bReadOk = false;
                            return false;
                        }
                        name = utf8_to_wstring(str);
                        if (nbnametag == 0) {
                            nbnametag++;
                        } else {
                            if (paramInputItems) {
                                delete paramInputItems;
                                paramInputItems = nullptr;
                            }
                            xmlFreeDoc(doc);
                            errorMessage.push_back(_W("line ") + std::to_wstring(nameNode->line)
                                + _W(": ") + utf8_to_wstring(PARAM_NAME_TAG) + L" "
                                + _W("duplicated."));
                            this->bReadOk = false;
                            return false;
                        }
                    }
                } else if (currentItemNodeName == PARAM_DESCRIPTION_TAG) {
                    xmlNodePtr descriptionNode = currentItemNode;
                    if (descriptionNode != nullptr) { //-V547
                        descriptionNode = descriptionNode->children;
                    }
                    if (descriptionNode) {
                        std::string str;
                        if (descriptionNode->content) {
                            str = std::string((char*)descriptionNode->content);
                        }
                        StringHelpers::trim(str);
                        if (str.empty()) {
                            if (paramInputItems) {
                                delete paramInputItems;
                                paramInputItems = nullptr;
                            }
                            xmlFreeDoc(doc);
                            errorMessage.push_back(_W("line ")
                                + std::to_wstring(descriptionNode->line) + _W(": ")
                                + utf8_to_wstring(PARAM_DESCRIPTION_TAG) + L" " + _W("is empty."));
                            this->bReadOk = false;
                            return false;
                        }
                        description = utf8_to_wstring(str);
                        if (nbdescriptiontag == 0) {
                            nbdescriptiontag++;
                        } else {
                            if (paramInputItems) {
                                delete paramInputItems;
                                paramInputItems = nullptr;
                            }
                            xmlFreeDoc(doc);
                            errorMessage.push_back(_W("line ")
                                + std::to_wstring(descriptionNode->line) + _W(": ")
                                + utf8_to_wstring(PARAM_DESCRIPTION_TAG) + L" "
                                + _W("duplicated."));
                            this->bReadOk = false;
                            return false;
                        }
                    }
                } else {
                    xmlFreeDoc(doc);
                    this->errorMessage.push_back(_W("line ")
                        + std::to_wstring(currentItemNode->line) + _W(": ")
                        + utf8_to_wstring(currentItemNodeName) + L" " + _W("not managed."));
                    this->bReadOk = false;
                    return false;
                }
                currentItemNode = currentItemNode->next;
            }
            if (nbdescriptiontag == 0) {
                if (paramInputItems) {
                    delete paramInputItems;
                    paramInputItems = nullptr;
                }
                xmlFreeDoc(doc);
                this->errorMessage.push_back(_W("line ") + std::to_wstring(paramInputItemNode->line)
                    + _W(": ") + utf8_to_wstring(PARAM_DESCRIPTION_TAG) + L" " + _W("missing."));
                this->bReadOk = false;
                return false;
            }
            if (nbnametag == 0) {
                if (paramInputItems) {
                    delete paramInputItems;
                    paramInputItems = nullptr;
                }
                xmlFreeDoc(doc);
                this->errorMessage.push_back(_W("line ") + std::to_wstring(paramInputItemNode->line)
                    + _W(": ") + utf8_to_wstring(PARAM_NAME_TAG) + L" " + _W("missing."));
                this->bReadOk = false;
                return false;
            }
            paramInputItems->append(name, description);
        }
        paramInputItemNode = paramInputItemNode->next;
    }
    items.push_back(paramInputItems);
    return true;
}
//=============================================================================
bool
XmlDocDocument::readFileCaseParamOutput(xmlDocPtr doc, xmlNodePtr node)
{
    XmlDocParamOutput* paramOutputItems = nullptr;
    if (node->children == nullptr) {
        xmlFreeDoc(doc);
        errorMessage.push_back(_W("line ") + std::to_wstring(node->line) + _W(": ")
            + utf8_to_wstring(PARAM_OUTPUT_TAG) + L" " + _W("has no child."));
        this->bReadOk = false;
        return false;
    }
    try {
        paramOutputItems = new XmlDocParamOutput();
    } catch (const std::bad_alloc&) {
        xmlFreeDoc(doc);
        errorMessage.push_back(_W("cannot allocate memory."));
        this->bReadOk = false;
        return false;
    }
    xmlNodePtr paramOutputItemNode = node->children;
    while (paramOutputItemNode != nullptr) {
        std::string currentNodeName = std::string((char*)paramOutputItemNode->name);
        if (currentNodeName == XML_COMMENT_TAG) {
            // NOTHING TO DO
        } else if (currentNodeName != PARAM_OUTPUT_ITEM_TAG) {
            xmlFreeDoc(doc);
            this->errorMessage.push_back(_W("line ") + std::to_wstring(paramOutputItemNode->line)
                + _W(": ") + utf8_to_wstring(currentNodeName) + L" " + _W("not managed."));
            this->bReadOk = false;
            return false;
        } else {
            std::wstring name = L"";
            std::wstring description = L"";
            int nbnametag = 0;
            int nbdescriptiontag = 0;
            xmlNodePtr currentItemNode = paramOutputItemNode->children;
            while (currentItemNode != nullptr) {
                std::string currentItemNodeName = std::string((char*)currentItemNode->name);
                if (currentItemNodeName == XML_COMMENT_TAG) {
                    // NOTHING TO DO :)
                } else if (currentItemNodeName == PARAM_NAME_TAG) {
                    xmlNodePtr nameNode = currentItemNode;
                    if (nameNode != nullptr) { //-V547
                        nameNode = nameNode->children;
                    }
                    if (nameNode) {
                        std::string str;
                        if (nameNode->content) {
                            str = std::string((char*)nameNode->content);
                        }
                        StringHelpers::trim(str);
                        if (str.empty()) {
                            if (paramOutputItems) {
                                delete paramOutputItems;
                                paramOutputItems = nullptr;
                            }
                            xmlFreeDoc(doc);
                            errorMessage.push_back(_W("line ") + std::to_wstring(nameNode->line)
                                + _W(": ") + utf8_to_wstring(PARAM_NAME_TAG) + L" "
                                + _W("is empty."));
                            this->bReadOk = false;
                            return false;
                        }
                        name = utf8_to_wstring(str);
                        if (nbnametag == 0) {
                            nbnametag++;
                        } else {
                            if (paramOutputItems) {
                                delete paramOutputItems;
                                paramOutputItems = nullptr;
                            }
                            xmlFreeDoc(doc);
                            errorMessage.push_back(_W("line ") + std::to_wstring(nameNode->line)
                                + _W(": ") + utf8_to_wstring(PARAM_NAME_TAG) + L" "
                                + _W("duplicated."));
                            this->bReadOk = false;
                            return false;
                        }
                    }
                } else if (currentItemNodeName == PARAM_DESCRIPTION_TAG) {
                    xmlNodePtr descriptionNode = currentItemNode;
                    if (descriptionNode != nullptr) { //-V547
                        descriptionNode = descriptionNode->children;
                    }
                    if (descriptionNode) {
                        std::string str;
                        if (descriptionNode->content) {
                            str = std::string((char*)descriptionNode->content);
                        }
                        StringHelpers::trim(str);
                        if (str.empty()) {
                            if (paramOutputItems) {
                                delete paramOutputItems;
                                paramOutputItems = nullptr;
                            }
                            xmlFreeDoc(doc);
                            errorMessage.push_back(_W("line ")
                                + std::to_wstring(descriptionNode->line) + _W(": ")
                                + utf8_to_wstring(PARAM_DESCRIPTION_TAG) + L" " + _W("is empty."));
                            this->bReadOk = false;
                            return false;
                        }
                        description = utf8_to_wstring(str);
                        if (nbdescriptiontag == 0) {
                            nbdescriptiontag++;
                        } else {
                            if (paramOutputItems) {
                                delete paramOutputItems;
                                paramOutputItems = nullptr;
                            }
                            xmlFreeDoc(doc);
                            errorMessage.push_back(_W("line ")
                                + std::to_wstring(descriptionNode->line) + _W(": ")
                                + utf8_to_wstring(PARAM_DESCRIPTION_TAG) + L" "
                                + _W("duplicated."));
                            this->bReadOk = false;
                            return false;
                        }
                    }
                } else {
                    xmlFreeDoc(doc);
                    this->errorMessage.push_back(_W("line ")
                        + std::to_wstring(currentItemNode->line) + _W(": ")
                        + utf8_to_wstring(currentItemNodeName) + L" " + _W("not managed."));
                    this->bReadOk = false;
                    return false;
                }
                currentItemNode = currentItemNode->next;
            }
            if (nbdescriptiontag == 0) {
                if (paramOutputItems) {
                    delete paramOutputItems;
                    paramOutputItems = nullptr;
                }
                xmlFreeDoc(doc);
                this->errorMessage.push_back(_W("line ")
                    + std::to_wstring(paramOutputItemNode->line) + _W(": ")
                    + utf8_to_wstring(PARAM_DESCRIPTION_TAG) + L" " + _W("missing."));
                this->bReadOk = false;
                return false;
            }
            if (nbnametag == 0) {
                if (paramOutputItems) {
                    delete paramOutputItems;
                    paramOutputItems = nullptr;
                }
                xmlFreeDoc(doc);
                this->errorMessage.push_back(_W("line ")
                    + std::to_wstring(paramOutputItemNode->line) + _W(": ")
                    + utf8_to_wstring(PARAM_NAME_TAG) + L" " + _W("missing."));
                this->bReadOk = false;
                return false;
            }
            paramOutputItems->append(name, description);
        }
        paramOutputItemNode = paramOutputItemNode->next;
    }
    items.push_back(paramOutputItems);
    return true;
}
//=============================================================================
bool
XmlDocDocument::readFileCaseExamples(xmlDocPtr doc, xmlNodePtr node)
{
    XmlDocExamples* examplesItems = nullptr;
    if (node->children == nullptr) {
        xmlFreeDoc(doc);
        errorMessage.push_back(_W("line ") + std::to_wstring(node->line) + _W(": ")
            + utf8_to_wstring(EXAMPLES_TAG) + L" " + _W("has no child."));
        this->bReadOk = false;
        return false;
    }
    try {
        examplesItems = new XmlDocExamples(this->outputTarget);
    } catch (const std::bad_alloc&) {
        xmlFreeDoc(doc);
        errorMessage.push_back(_W("cannot allocate memory."));
        this->bReadOk = false;
        return false;
    }
    xmlNodePtr exampleItemNode = node->children;
    while (exampleItemNode != nullptr) {
        std::string currentNodeName = std::string((char*)exampleItemNode->name);
        if (currentNodeName == XML_COMMENT_TAG) {
            // NOTHING TO DO
        } else if (currentNodeName != EXAMPLE_ITEM_TAG) {
            xmlFreeDoc(doc);
            this->errorMessage.push_back(_W("line ") + std::to_wstring(exampleItemNode->line)
                + _W(": ") + utf8_to_wstring(currentNodeName) + L" " + _W("not managed."));
            this->bReadOk = false;
            return false;
        } else {
            std::wstring type = L"";
            std::wstring description = L"";
            std::wstring data = L"";
            std::wstring imagetag = L"";
            int nbtypetag = 0;
            int nbdescriptiontag = 0;
            int nbdatatag = 0;
            int nbimgtag = 0;
            xmlNodePtr currentItemNode = exampleItemNode->children;
            while (currentItemNode != nullptr) {
                std::string currentItemNodeName = std::string((char*)currentItemNode->name);
                if (currentItemNodeName == XML_COMMENT_TAG) {
                    // NOTHING TO DO :)
                } else if (currentItemNodeName == EXAMPLE_ITEM_TYPE_TAG) {
                    xmlNodePtr nameNode = currentItemNode;
                    if (nameNode != nullptr) { //-V547
                        nameNode = nameNode->children;
                    }
                    if (nameNode) {
                        std::string str;
                        if (nameNode->content) {
                            str = std::string((char*)nameNode->content);
                        }
                        StringHelpers::trim(str);
                        if (str.empty()) {
                            if (examplesItems) {
                                delete examplesItems;
                                examplesItems = nullptr;
                            }
                            xmlFreeDoc(doc);
                            errorMessage.push_back(_W("line ") + std::to_wstring(nameNode->line)
                                + _W(": ") + utf8_to_wstring(EXAMPLE_ITEM_TYPE_TAG) + L" "
                                + _W("is empty."));
                            this->bReadOk = false;
                            return false;
                        }
                        type = utf8_to_wstring(str);
                        if (nbtypetag == 0) {
                            nbtypetag++;
                        } else {
                            if (examplesItems) {
                                delete examplesItems;
                                examplesItems = nullptr;
                            }
                            xmlFreeDoc(doc);
                            errorMessage.push_back(_W("line ") + std::to_wstring(nameNode->line)
                                + _W(": ") + utf8_to_wstring(EXAMPLE_ITEM_TYPE_TAG) + L" "
                                + _W("duplicated."));
                            this->bReadOk = false;
                            return false;
                        }
                    }
                } else if (currentItemNodeName == EXAMPLE_ITEM_DESCRIPTION_TAG) {
                    xmlNodePtr descriptionNode = currentItemNode;
                    if (descriptionNode != nullptr) { //-V547
                        descriptionNode = descriptionNode->children;
                    }
                    if (descriptionNode) {
                        std::string str;
                        if (descriptionNode->content) {
                            str = std::string((char*)descriptionNode->content);
                        }
                        StringHelpers::trim(str);
                        if (str.empty()) {
                            warningMessage.push_back(_W("line ")
                                + std::to_wstring(descriptionNode->line) + _W(": ")
                                + utf8_to_wstring(EXAMPLE_ITEM_DESCRIPTION_TAG) + L" "
                                + _W("is empty."));
                        }
                        description = utf8_to_wstring(str);
                        if (nbdescriptiontag == 0) {
                            nbdescriptiontag++;
                        } else {
                            if (examplesItems) {
                                delete examplesItems;
                                examplesItems = nullptr;
                            }
                            xmlFreeDoc(doc);
                            errorMessage.push_back(_W("line ")
                                + std::to_wstring(descriptionNode->line) + _W(": ")
                                + utf8_to_wstring(EXAMPLE_ITEM_DESCRIPTION_TAG) + L" "
                                + _W("duplicated."));
                            this->bReadOk = false;
                            return false;
                        }
                    } else {
                        warningMessage.push_back(_W("line ")
                            + std::to_wstring(currentItemNode->line) + _W(": ")
                            + utf8_to_wstring(EXAMPLE_ITEM_DESCRIPTION_TAG) + L" "
                            + _W("is empty."));
                        description.clear();
                        if (nbdescriptiontag == 0) {
                            nbdescriptiontag++;
                        } else {
                            if (examplesItems) {
                                delete examplesItems;
                                examplesItems = nullptr;
                            }
                            xmlFreeDoc(doc);
                            errorMessage.push_back(_W("line ")
                                + std::to_wstring(currentItemNode->line) + _W(": ")
                                + utf8_to_wstring(EXAMPLE_ITEM_DESCRIPTION_TAG) + L" "
                                + _W("duplicated."));
                            this->bReadOk = false;
                            return false;
                        }
                    }
                } else if (currentItemNodeName == EXAMPLE_ITEM_IMG_TAG) {
                    xmlNodePtr dataNode = currentItemNode;
                    xmlBufferPtr buffer = xmlBufferCreate();
                    xmlNodePtr imageNode = currentItemNode;
                    xmlNodeDump(buffer, doc, imageNode, 0, 1);
                    std::string str = std::string((char*)buffer->content);
                    xmlBufferFree(buffer);
                    StringHelpers::trim(str);
                    StringHelpers::replace_all(str, EXAMPLE_ITEM_IMG_TAG, IMAGE_TAG);
                    imagetag = utf8_to_wstring(str);
                    if (!isValidImageTag(imagetag)) {
                        if (examplesItems) {
                            delete examplesItems;
                            examplesItems = nullptr;
                        }
                        xmlFreeDoc(doc);
                        errorMessage.push_back(_W("line ") + std::to_wstring(dataNode->line)
                            + _W(": ") + utf8_to_wstring(EXAMPLE_ITEM_IMG_TAG) + L" "
                            + _W("malformed."));
                        this->bReadOk = false;
                        return false;
                    }
                    std::wstring newPath;
                    std::wstring oldPath;
                    if (!parseImageTag(imagetag, this->xmlDirectory, oldPath, newPath)) {
                        if (examplesItems) {
                            delete examplesItems;
                            examplesItems = nullptr;
                        }
                        xmlFreeDoc(doc);
                        errorMessage.push_back(_W("line ") + std::to_wstring(dataNode->line)
                            + _W(": ") + utf8_to_wstring(EXAMPLE_ITEM_IMG_TAG) + L" "
                            + _W("file does not exist."));
                        this->bReadOk = false;
                        return false;
                    }
                    if (nbimgtag == 0) {
                        nbimgtag++;
                    } else {
                        if (examplesItems) {
                            delete examplesItems;
                            examplesItems = nullptr;
                        }
                        xmlFreeDoc(doc);
                        errorMessage.push_back(_W("line ") + std::to_wstring(dataNode->line)
                            + _W(": ") + utf8_to_wstring(EXAMPLE_ITEM_IMG_TAG) + L" "
                            + _W("duplicated."));
                        this->bReadOk = false;
                        return false;
                    }
                } else if (currentItemNodeName == EXAMPLE_ITEM_DATA_TAG) {
                    xmlNodePtr dataNode = currentItemNode;
                    if (dataNode != nullptr) { //-V547
                        dataNode = dataNode->children;
                    }
                    if (dataNode) {
                        std::string str;
                        if (dataNode->content) {
                            str = std::string((char*)dataNode->content);
                        }
                        StringHelpers::trim(str);
                        if (str.empty()) {
                            if (examplesItems) {
                                delete examplesItems;
                                examplesItems = nullptr;
                            }
                            xmlFreeDoc(doc);
                            errorMessage.push_back(_W("line ") + std::to_wstring(dataNode->line)
                                + _W(": ") + utf8_to_wstring(EXAMPLE_ITEM_DATA_TAG) + L" "
                                + _W("is empty."));
                            this->bReadOk = false;
                            return false;
                        }
                        data = utf8_to_wstring(str);
                        if (nbdatatag == 0) {
                            nbdatatag++;
                        } else {
                            if (examplesItems) {
                                delete examplesItems;
                                examplesItems = nullptr;
                            }
                            xmlFreeDoc(doc);
                            errorMessage.push_back(_W("line ") + std::to_wstring(dataNode->line)
                                + _W(": ") + utf8_to_wstring(EXAMPLE_ITEM_DATA_TAG) + L" "
                                + _W("duplicated."));
                            this->bReadOk = false;
                            return false;
                        }
                    }
                } else {
                    xmlFreeDoc(doc);
                    this->errorMessage.push_back(_W("line ")
                        + std::to_wstring(currentItemNode->line) + _W(": ")
                        + utf8_to_wstring(currentItemNodeName) + L" " + _W("not managed."));
                    this->bReadOk = false;
                    return false;
                }
                currentItemNode = currentItemNode->next;
            }
            if (nbtypetag == 0) {
                if (examplesItems) {
                    delete examplesItems;
                    examplesItems = nullptr;
                }
                if (currentItemNode) { //-V547
                    this->errorMessage.push_back(_W("line ")
                        + std::to_wstring(currentItemNode->line) + _W(": ")
                        + utf8_to_wstring(EXAMPLE_ITEM_TYPE_TAG) + L" " + _W("missing."));
                } else {
                    this->errorMessage.push_back(
                        utf8_to_wstring(EXAMPLE_ITEM_TYPE_TAG) + L" " + _W("missing."));
                }

                this->bReadOk = false;
                xmlFreeDoc(doc);
                return false;
            }
            if (nbdescriptiontag == 0) {
                if (examplesItems) {
                    delete examplesItems;
                    examplesItems = nullptr;
                }
                xmlFreeDoc(doc);
                this->errorMessage.push_back(_W("line ")
                    + std::to_wstring(currentItemNode->line) //-V522
                    + _W(": ") + utf8_to_wstring(EXAMPLE_ITEM_DESCRIPTION_TAG) + L" "
                    + _W("missing."));
                this->bReadOk = false;
                return false;
            }
            if (nbdatatag == 0) {
                if (examplesItems) {
                    delete examplesItems;
                    examplesItems = nullptr;
                }
                xmlFreeDoc(doc);
                this->errorMessage.push_back(_W("line ") + std::to_wstring(currentItemNode->line)
                    + _W(": ") + utf8_to_wstring(EXAMPLE_ITEM_DATA_TAG) + L" " + _W("missing."));
                this->bReadOk = false;
                return false;
            }
            examplesItems->append(type, description, data, imagetag);
        }
        exampleItemNode = exampleItemNode->next;
    }
    examplesItems->setDirectories(this->xmlDirectory, this->directoryDestination);
    items.push_back(examplesItems);
    return true;
}
//=============================================================================
bool
XmlDocDocument::readFileCaseSeeAlso(xmlDocPtr doc, xmlNodePtr node)
{
    XmlDocSeeAlso* seeAlsoItems = nullptr;
    if (node->children == nullptr) {
        xmlFreeDoc(doc);
        errorMessage.push_back(_W("line ") + std::to_wstring(node->line) + _W(": ")
            + utf8_to_wstring(SEE_ALSO_TAG) + L" " + _W("has no child."));
        this->bReadOk = false;
        return false;
    }
    try {
        seeAlsoItems = new XmlDocSeeAlso();
    } catch (const std::bad_alloc&) {
        xmlFreeDoc(doc);
        errorMessage.push_back(_W("cannot allocate memory."));
        this->bReadOk = false;
        return false;
    }
    xmlNodePtr seeAlsoItemNode = node->children;
    while (seeAlsoItemNode != nullptr) {
        std::string currentNodeName = std::string((char*)seeAlsoItemNode->name);
        if (currentNodeName == XML_COMMENT_TAG) {
            // NOTHING TO DO
        } else if (currentNodeName != SEE_ALSO_ITEM_TAG) {
            if (seeAlsoItems) {
                delete seeAlsoItems;
                seeAlsoItems = nullptr;
            }
            xmlFreeDoc(doc);
            this->errorMessage.push_back(_W("line ") + std::to_wstring(seeAlsoItemNode->line)
                + _W(": ") + utf8_to_wstring(currentNodeName) + L" " + _W("not managed."));
            this->bReadOk = false;
            return false;
        } else {
            std::wstring name = L"";
            std::wstring link = L"";
            xmlNodePtr currentItemNode = seeAlsoItemNode->children;
            size_t nbLinksItems = 0;
            while (currentItemNode != nullptr) {
                std::string currentItemNodeName = std::string((char*)currentItemNode->name);
                if (nbLinksItems > 0) {
                    if (seeAlsoItems) {
                        delete seeAlsoItems;
                        seeAlsoItems = nullptr;
                    }
                    xmlFreeDoc(doc);
                    errorMessage.push_back(_W("line ") + std::to_wstring(currentItemNode->line)
                        + _W(": ") + utf8_to_wstring(XML_LINK_TAG) + L" " + _W("duplicated."));
                    this->bReadOk = false;
                    return false;
                }
                if (currentItemNodeName == XML_COMMENT_TAG) {
                    // NOTHING TO DO :)
                } else if (currentItemNodeName == XML_LINK_TAG) {
                    if (!readFileCaseLink(doc, currentItemNode, name, link)) {
                        if (seeAlsoItems) {
                            delete seeAlsoItems;
                            seeAlsoItems = nullptr;
                        }
                        return false;
                    }
                } else {
                    if (seeAlsoItems) {
                        delete seeAlsoItems;
                        seeAlsoItems = nullptr;
                    }
                    xmlFreeDoc(doc);
                    this->errorMessage.push_back(_W("line ")
                        + std::to_wstring(seeAlsoItemNode->line) + _W(": ")
                        + utf8_to_wstring(currentNodeName) + L" " + _W("not managed."));
                    this->bReadOk = false;
                    return false;
                }
                if (!XmlDocCheckIfLinkExists(this->xmlDirectory, link, this->getLanguage())) {
                    this->warningMessage.push_back(_W("line ")
                        + std::to_wstring(seeAlsoItemNode->line) + _W(": ") + link + L" "
                        + _W("not found."));
                }
                XmlDocResolveLink(this->xmlDirectory, link, this->sectionName, this->outputTarget,
                    this->directoryDestination, getLanguage(), link);
                seeAlsoItems->append(name, link);
                nbLinksItems++;
                currentItemNode = currentItemNode->next;
            }
        }
        seeAlsoItemNode = seeAlsoItemNode->next;
    }
    items.push_back(seeAlsoItems);
    return true;
}
//=============================================================================
bool
XmlDocDocument::readFileCaseChapter(xmlDocPtr doc, xmlNodePtr node)
{
    if (node->content) {
        xmlFreeDoc(doc);
        errorMessage.push_back(_W("line ") + std::to_wstring(node->line) + _W(": ")
            + utf8_to_wstring(CHAPTER_TAG) + L" " + _W("invalid."));
        this->bReadOk = false;
        return false;
    }
    xmlNodePtr chapterNode = node->children;
    if (chapterNode->children) {
        xmlFreeDoc(doc);
        errorMessage.push_back(_W("line ") + std::to_wstring(chapterNode->line) + _W(": ")
            + utf8_to_wstring(CHAPTER_TAG) + L" " + _W("has child."));
        this->bReadOk = false;
        return false;
    }
    std::string str;
    if (chapterNode->content) {
        str = std::string((char*)chapterNode->content);
    }
    StringHelpers::trim(str);
    if (str.empty()) {
        xmlFreeDoc(doc);
        errorMessage.push_back(_W("line ") + std::to_wstring(chapterNode->line) + _W(": ")
            + utf8_to_wstring(CHAPTER_TAG) + L" " + _W("is empty."));
        this->bReadOk = false;
        return false;
    }
    items.push_back(new XmlDocChapterItem(utf8_to_wstring(str)));
    return true;
}
//=============================================================================
bool
XmlDocDocument::readFileCaseChapterDescription(xmlDocPtr doc, xmlNodePtr node)
{
    if (node->content) {
        xmlFreeDoc(doc);
        errorMessage.push_back(_W("line ") + std::to_wstring(node->line) + _W(": ")
            + utf8_to_wstring(CHAPTER_DESCRIPTION_TAG) + L" " + _W("invalid."));
        this->bReadOk = false;
        return false;
    }
    xmlNodePtr chapterDescriptionNode = node->children;
    if (chapterDescriptionNode) {
        if (chapterDescriptionNode->children) {
            xmlFreeDoc(doc);
            errorMessage.push_back(_W("line ") + std::to_wstring(chapterDescriptionNode->line)
                + _W(": ") + utf8_to_wstring(CHAPTER_DESCRIPTION_TAG) + L" " + _W("has child."));
            this->bReadOk = false;
            return false;
        }
        std::string str;
        if (chapterDescriptionNode->content) {
            str = std::string((char*)chapterDescriptionNode->content);
        }
        StringHelpers::trim(str);
        if (str.empty()) {
            warningMessage.push_back(_W("line ") + std::to_wstring(chapterDescriptionNode->line)
                + _W(": ") + utf8_to_wstring(CHAPTER_DESCRIPTION_TAG) + L" " + _W("is empty."));
        } else {
            items.push_back(new XmlDocChapterDescriptionItem(utf8_to_wstring(str)));
        }
    } else {
        warningMessage.push_back(_W("line ") + std::to_wstring(node->line) + _W(": ")
            + utf8_to_wstring(CHAPTER_DESCRIPTION_TAG) + L" " + _W("is empty."));
    }

    return true;
}
//=============================================================================
bool
XmlDocDocument::readFileCaseChapterIndex(xmlDocPtr doc, xmlNodePtr node)
{
    XmlDocChapterIndexItem* chapterIndexItem = nullptr;
    if (node->children == nullptr) {
        xmlFreeDoc(doc);
        errorMessage.push_back(_W("line ") + std::to_wstring(node->line) + _W(": ")
            + utf8_to_wstring(CHAPTER_INDEX_TAG) + L" " + _W("has no child."));
        this->bReadOk = false;
        return false;
    }
    try {
        chapterIndexItem = new XmlDocChapterIndexItem();
    } catch (const std::bad_alloc&) {
        xmlFreeDoc(doc);
        errorMessage.push_back(_W("cannot allocate memory."));
        this->bReadOk = false;
        return false;
    }
    xmlNodePtr chapterIndexItemNode = node->children;
    while (chapterIndexItemNode != nullptr) {
        std::string currentNodeName = std::string((char*)chapterIndexItemNode->name);
        if (currentNodeName == XML_COMMENT_TAG) {
            // NOTHING TO DO
        } else if (currentNodeName != CHAPTER_REF_TAG) {
            xmlFreeDoc(doc);
            this->errorMessage.push_back(_W("line ") + std::to_wstring(chapterIndexItemNode->line)
                + _W(": ") + utf8_to_wstring(currentNodeName) + L" " + _W("not managed."));
            this->bReadOk = false;
            return false;
        } else {
            std::wstring linkname = L"";
            std::wstring linkurl = L"";
            std::wstring linkdescription = L"";
            int nblinktag = 0;
            int nbdescriptiontag = 0;
            xmlNodePtr currentItemNode = chapterIndexItemNode->children;
            while (currentItemNode != nullptr) {
                std::string currentItemNodeName = std::string((char*)currentItemNode->name);
                if (currentItemNodeName == XML_COMMENT_TAG) {
                    // NOTHING TO DO :)
                } else if (currentItemNodeName == XML_LINK_TAG) {
                    if (!readFileCaseLink(doc, currentItemNode, linkname, linkurl)) {
                        if (chapterIndexItem) {
                            delete chapterIndexItem;
                            chapterIndexItem = nullptr;
                        }
                        return false;
                    }
                    if (nblinktag == 0) {
                        nblinktag++;
                    } else {
                        if (chapterIndexItem) {
                            delete chapterIndexItem;
                            chapterIndexItem = nullptr;
                        }
                        xmlFreeDoc(doc);
                        errorMessage.push_back(_W("line ") + std::to_wstring(currentItemNode->line)
                            + _W(": ") + utf8_to_wstring(XML_LINK_TAG) + L" " + _W("duplicated."));
                        this->bReadOk = false;
                        return false;
                    }

                } else if (currentItemNodeName == CHAPTER_REF_DESCRIPTION_TAG) {
                    xmlNodePtr descriptionNode = currentItemNode;
                    if (descriptionNode != nullptr) { //-V547
                        descriptionNode = descriptionNode->children;
                    }
                    if (descriptionNode) {
                        std::string str;
                        if (descriptionNode->content) {
                            str = std::string((char*)descriptionNode->content);
                        }
                        StringHelpers::trim(str);
                        if (str.empty()) {
                            if (chapterIndexItem) {
                                delete chapterIndexItem;
                                chapterIndexItem = nullptr;
                            }
                            xmlFreeDoc(doc);
                            errorMessage.push_back(_W("line ")
                                + std::to_wstring(descriptionNode->line) + _W(": ")
                                + utf8_to_wstring(CHAPTER_REF_DESCRIPTION_TAG) + L" "
                                + _W("is empty."));
                            this->bReadOk = false;
                            return false;
                        }
                        linkdescription = utf8_to_wstring(str);
                        if (nbdescriptiontag == 0) {
                            nbdescriptiontag++;
                        } else {
                            if (chapterIndexItem) {
                                delete chapterIndexItem;
                                chapterIndexItem = nullptr;
                            }
                            xmlFreeDoc(doc);
                            errorMessage.push_back(_W("line ")
                                + std::to_wstring(descriptionNode->line) + _W(": ")
                                + utf8_to_wstring(CHAPTER_REF_DESCRIPTION_TAG) + L" "
                                + _W("duplicated."));
                            this->bReadOk = false;
                            return false;
                        }
                    }
                } else {
                    xmlFreeDoc(doc);
                    this->errorMessage.push_back(_W("line ")
                        + std::to_wstring(currentItemNode->line) + _W(": ")
                        + utf8_to_wstring(currentItemNodeName) + L" " + _W("not managed."));
                    this->bReadOk = false;
                    return false;
                }
                currentItemNode = currentItemNode->next;
            }
            chapterIndexItem->append(linkname, linkurl, linkdescription);
        }
        chapterIndexItemNode = chapterIndexItemNode->next;
    }
    items.push_back(chapterIndexItem);
    return true;
}
//=============================================================================
bool
XmlDocDocument::readFileCaseLink(
    xmlDocPtr doc, xmlNodePtr node, std::wstring& name, std::wstring& url)
{
    name.clear();
    url.clear();
    if (node == nullptr) {
        return false;
    }
    xmlNodePtr linkItemNode = node->children;
    if (node->properties) {
        std::string strlinkend;
        if (node->properties->name) {
            strlinkend = std::string((char*)node->properties->name);
        } else {
            xmlFreeDoc(doc);
            if (node) { //-V547
                this->errorMessage.push_back(_W("line ") + std::to_wstring(node->line) + _W(": ")
                    + utf8_to_wstring(XML_LINK_TAG) + L" " + _W("has no property."));
            } else {
                this->errorMessage.push_back(
                    utf8_to_wstring(XML_LINK_TAG) + L" " + _W("has no property."));
            }
            this->bReadOk = false;
            return false;
        }
        if (strlinkend == XML_LINKEND_TAG) {
            xmlNodePtr linkendItemNode = node->properties->children;
            if (linkendItemNode) {
                std::string val;
                val = std::string((char*)linkendItemNode->content);
                if (val.empty()) {
                    xmlFreeDoc(doc);
                    this->errorMessage.push_back(_W("line ")
                        + std::to_wstring(linkendItemNode->line) + _W(": ")
                        + utf8_to_wstring(XML_LINKEND_TAG) + L" " + _W("is empty."));
                    this->bReadOk = false;
                    xmlFreeDoc(doc);
                    return false;
                }
                url = utf8_to_wstring(val);
            } else {
                this->errorMessage.push_back(
                    utf8_to_wstring(XML_LINKEND_TAG) + L" " + _W("has no property."));
                xmlFreeDoc(doc);
                this->bReadOk = false;
                return false;
            }
        }
    }
    if (linkItemNode) {
        std::string str;
        if (linkItemNode->content) {
            str = std::string((char*)linkItemNode->content);
        }
        if (str.empty()) {
            xmlFreeDoc(doc);
            this->errorMessage.push_back(_W("line ") + std::to_wstring(linkItemNode->line)
                + _W(": ") + utf8_to_wstring(XML_LINKEND_TAG) + L" " + _W("is empty."));
            this->bReadOk = false;
            return false;
        }
        name = utf8_to_wstring(str);
    } else {
        this->errorMessage.push_back(_W("line ") + std::to_wstring(linkItemNode->line)
            + _W(": ") //-V522
            + utf8_to_wstring(XML_LINK_TAG) + L" " + _W("has no property."));
        xmlFreeDoc(doc);
        this->bReadOk = false;
        return false;
    }
    return true;
}
//=============================================================================
bool
XmlDocDocument::readFileCaseImage(xmlDocPtr doc, xmlNodePtr node)
{
    xmlBufferPtr buffer = xmlBufferCreate();
    xmlNodePtr imageNode = node;
    xmlNodeDump(buffer, doc, imageNode, 0, 1);
    std::string str = std::string((char*)buffer->content);
    xmlBufferFree(buffer);
    StringHelpers::trim(str);
    if (str.empty()) {
        errorMessage.push_back(_W("line ") + std::to_wstring(node->line) + _W(": ")
            + utf8_to_wstring(IMAGE_TAG) + L" " + _W("is empty."));
        xmlFreeDoc(doc);
        this->bReadOk = false;
        return false;
    }
    XmlDocImageItem* ptrImageItem = new XmlDocImageItem(utf8_to_wstring(str));
    ptrImageItem->setDirectories(this->xmlDirectory, this->directoryDestination);
    try {
        ptrImageItem->findImage();
    } catch (Exception& e) {
        errorMessage.push_back(utf8_to_wstring(IMAGE_TAG) + L": " + e.getMessage());
        this->bReadOk = false;
        delete ptrImageItem;
        return false;
    }
    items.push_back(ptrImageItem);
    return true;
}
//=============================================================================
bool
XmlDocDocument::readFileCaseModuleName(xmlDocPtr doc, xmlNodePtr node)
{
    if (node->content) {
        xmlFreeDoc(doc);
        errorMessage.push_back(_W("line ") + std::to_wstring(node->line) + _W(": ")
            + utf8_to_wstring(MODULE_NAME_TAG) + L" " + _W("invalid."));
        this->bReadOk = false;
        return false;
    }
    xmlNodePtr moduleNameNode = node->children;
    if (moduleNameNode->children) {
        xmlFreeDoc(doc);
        errorMessage.push_back(_W("line ") + std::to_wstring(moduleNameNode->line) + _W(": ")
            + utf8_to_wstring(MODULE_NAME_TAG) + L" " + _W("has child."));
        this->bReadOk = false;
        return false;
    }
    std::string str;
    if (moduleNameNode->content) {
        str = std::string((char*)moduleNameNode->content);
    }
    StringHelpers::trim(str);
    if (str.empty()) {
        xmlFreeDoc(doc);
        errorMessage.push_back(_W("line ") + std::to_wstring(moduleNameNode->line) + _W(": ")
            + utf8_to_wstring(MODULE_NAME_TAG) + L" " + _W("is empty."));
        this->bReadOk = false;
        return false;
    }
    items.push_back(new XmlDocModuleNameItem(utf8_to_wstring(str)));
    return true;
}
//=============================================================================
void
XmlDocDocument::setDestinationFile(const std::wstring& _filenameDestination)
{
    this->filenameDestination = _filenameDestination;
    this->directoryDestination = L"./";
    FileSystemWrapper::Path pathToSplit(_filenameDestination);
    if (pathToSplit.has_parent_path()) {
        this->directoryDestination = pathToSplit.parent_path().generic_wstring();
        if (this->directoryDestination[this->directoryDestination.size() - 1] != '/'
            && this->directoryDestination[this->directoryDestination.size() - 1] != '\\') {
            this->directoryDestination.push_back('/');
        }
    }
}
//=============================================================================
std::wstring
XmlDocDocument::getDestinationFile()
{
    return this->filenameDestination;
}
//=============================================================================
bool
XmlDocDocument::copyHtmlDependencies()
{
    std::wstring helpToolsPath = GetModulePath(L"help_tools");
    if (!helpToolsPath.empty()) {
        std::wstring ressourcesPath = helpToolsPath + L"/resources/";
        wstringVector files;
        files.push_back(L"highlight.pack.js");
        files.push_back(L"style.css");
        files.push_back(L"mono-blue.css");
        for (auto& file : files) {
            FileSystemWrapper::Path dstFile = this->directoryDestination;
            dstFile = dstFile / file;
            if (!FileSystemWrapper::Path::exists(dstFile)) {
                FileSystemWrapper::Path srcFile = ressourcesPath;
                srcFile = srcFile / file;
                bool bIsFile = FileSystemWrapper::Path::is_regular_file(srcFile);
                if (bIsFile) {
                    FileSystemWrapper::Path::copy_file(srcFile, dstFile);
                }
            }
        }
    }
    return true;
}
//=============================================================================
bool
XmlDocDocument::needToUpdate()
{
    if (this->bOverwriteExistingFile) {
        return true;
    }
    if (FileSystemWrapper::Path::exists(this->filenameDestination)) {
        std::time_t t1 = FileSystemWrapper::Path::last_write_time(this->xmlfilename);
        std::time_t t2 = FileSystemWrapper::Path::last_write_time(this->filenameDestination);
        if (t1 >= t2) {
            return true;
        }
        return false;
    }
    return true;
}
//=============================================================================
bool
XmlDocDocument::writeAsMarkdown()
{
    bool res = true;
    if (needToUpdate()) {
        std::ofstream markdownFile;
        std::string markdownUtf8Stream = "";
        std::wstring nelsonLanguage = Localization::Instance()->getCurrentLanguage();
        Localization::Instance()->setLanguage(this->getLanguage(), false);
        res = this->writeAsMarkdown(markdownUtf8Stream);
        Localization::Instance()->setLanguage(nelsonLanguage, false);
        if (res) {
#if _MSC_VER
            markdownFile.open(this->filenameDestination);
#else
            markdownFile.open(wstring_to_utf8(this->filenameDestination));
#endif
            if (markdownFile.is_open()) {
                markdownFile << markdownUtf8Stream << std::endl;
                markdownFile.close();
                res = true; //-V1048
            } else {
                res = false;
            }
        }
    }
    return res;
}
//=============================================================================
bool
XmlDocDocument::writeAsHtml()
{
    bool res = true;
    if (needToUpdate()) {
        std::ofstream htmlfile;
        std::string htmlUtf8Stream = "";
        std::wstring nelsonLanguage = Localization::Instance()->getCurrentLanguage();
        Localization::Instance()->setLanguage(this->getLanguage(), false);
        res = this->writeAsHtml(htmlUtf8Stream);
        Localization::Instance()->setLanguage(nelsonLanguage, false);
        if (res) {
#if _MSC_VER
            htmlfile.open(this->filenameDestination);
#else
            htmlfile.open(wstring_to_utf8(this->filenameDestination));
#endif
            if (htmlfile.is_open()) {
                copyHtmlDependencies();
                htmlfile << htmlUtf8Stream << std::endl;
                htmlfile.close();
                res = true; //-V1048
            } else {
                res = false;
            }
        }
    }
    return res;
}
//=============================================================================
void
XmlDocDocument::setPreviousPageLink(const std::wstring& linkname, const std::wstring& linkurl)
{
    this->previousLinkName = linkname;
    this->previousLinkUrl = linkurl;
}
//=============================================================================
void
XmlDocDocument::setNextPageLink(const std::wstring& linkname, const std::wstring& linkurl)
{
    this->nextLinkName = linkname;
    this->nextLinkUrl = linkurl;
}
//=============================================================================
void
XmlDocDocument::setIndexPageLink(const std::wstring& linkname, const std::wstring& linkurl)
{
    this->indexLinkName = linkname;
    this->indexLinkUrl = linkurl;
}
//=============================================================================
std::wstring
XmlDocDocument::getChapterDescription()
{
    XmlDocGenericItem* pItem = findfirst(CHAPTER_DESCRIPTION_TAG);
    if (pItem) {
        XmlDocChapterDescriptionItem* pChapterDescriptionItem
            = (XmlDocChapterDescriptionItem*)pItem;
        return pChapterDescriptionItem->getValue();
    }
    return L"";
}
//=============================================================================
std::wstring
XmlDocDocument::getModuleName()
{
    XmlDocGenericItem* pItem = findfirst(MODULE_NAME_TAG);
    if (pItem) {
        XmlDocModuleNameItem* pModuleNameItem = (XmlDocModuleNameItem*)pItem;
        return pModuleNameItem->getValue();
    }
    return L"";
}
//=============================================================================
}
//=============================================================================
