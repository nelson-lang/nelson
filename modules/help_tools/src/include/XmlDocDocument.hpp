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
#pragma once
//=============================================================================
#include "Types.hpp"
#include "XmlDocGenericItem.hpp"
#include "XmlDocumentTags.hpp"
#include "XmlTarget.hpp"
#include "nlsHelp_tools_exports.h"
#include <boost/container/vector.hpp>
#include <libxml/xpath.h>
#include <string>
//=============================================================================
namespace Nelson {
//=============================================================================
class NLSHELP_TOOLS_IMPEXP XmlDocDocument
{

private:
    std::wstring filenameDestination;
    std::wstring directoryDestination;

    std::wstring xmlfilename;
    std::wstring xmlDirectory;
    wstringVector errorMessage;
    wstringVector warningMessage;
    boost::container::vector<XmlDocGenericItem*> items;
    void
    clearItems();
    bool bReadOk;
    bool bOverwriteExistingFile;

    std::wstring previousLinkName;
    std::wstring previousLinkUrl;

    std::wstring nextLinkName;
    std::wstring nextLinkUrl;

    std::wstring indexLinkName;
    std::wstring indexLinkUrl;

    std::wstring sectionName;

    DOCUMENT_OUTPUT outputTarget;

public:
    XmlDocDocument(std::wstring srcfilename, std::wstring sectionname, std::wstring destfilename,
        bool bOverwriteExistingFile = false, DOCUMENT_OUTPUT outputTarget = DOCUMENT_OUTPUT::HMTL);
    XmlDocDocument(boost::container::vector<XmlDocGenericItem*> items, std::wstring srcfilename,
        std::wstring destfilename, bool bOverwriteExistingFile = false,
        DOCUMENT_OUTPUT outputTarget = DOCUMENT_OUTPUT::HMTL);
    ~XmlDocDocument();

    wstringVector
    getError();
    wstringVector
    getWarning();
    std::wstring
    getFilename();

    bool
    isKeywordDocument();
    bool
    isChapterDocument();
    bool
    isTitleDocument();

    bool
    readFile();
    bool
    writeAsHtml(std::string& utf8stream);
    bool
    writeAsHtml();
    bool
    writeAsMarkdown(std::string& utf8stream);
    bool
    writeAsMarkdown();

    std::wstring
    getDestinationFile();
    bool
    needToUpdate();

    void
    setPreviousPageLink(std::wstring linkname, std::wstring linkurl);
    void
    setNextPageLink(std::wstring linkname, std::wstring linkurl);
    void
    setIndexPageLink(std::wstring linkname, std::wstring linkurl);

    std::wstring
    getPageTitle();
    std::wstring
    getPageDescription();

    std::wstring
    getChapter();
    std::wstring
    getChapterDescription();
    std::wstring
    getModuleName();

    boost::container::vector<XmlDocGenericItem*>
    getXmlDocGenericItems();

private:
    enum XMLDOC_ITEMS_ID
    stringTagToId(std::string tag);
    XmlDocGenericItem*
    findfirst(std::string tag);
    size_t
    count(std::string tag);
    std::wstring
    getLanguage();
    std::wstring
    getTitle();
    std::wstring
    getKeyword();
    std::wstring
    getCopyright();
    std::wstring
    getShortDescription();
    bool
    readFileCaseUsedFunction(xmlDocPtr doc, xmlNodePtr node);
    bool
    readFileCaseSyntax(xmlDocPtr doc, xmlNodePtr node);
    bool
    readFileCaseLanguage(xmlDocPtr doc, xmlNodePtr node);
    bool
    readFileCaseTitle(xmlDocPtr doc, xmlNodePtr node);
    bool
    readFileCaseKeyword(xmlDocPtr doc, xmlNodePtr node);
    bool
    readFileCaseCopyright(xmlDocPtr doc, xmlNodePtr node);
    bool
    readFileCaseDescription(xmlDocPtr doc, xmlNodePtr node);
    bool
    readFileCaseShortDescription(xmlDocPtr doc, xmlNodePtr node);
    bool
    readFileCaseBibliography(xmlDocPtr doc, xmlNodePtr node);
    bool
    readFileCaseAuthors(xmlDocPtr doc, xmlNodePtr node);
    bool
    readFileCaseHistory(xmlDocPtr doc, xmlNodePtr node);
    bool
    readFileCaseParamInput(xmlDocPtr doc, xmlNodePtr node);
    bool
    readFileCaseParamOutput(xmlDocPtr doc, xmlNodePtr node);
    bool
    readFileCaseExamples(xmlDocPtr doc, xmlNodePtr node);
    bool
    readFileCaseSeeAlso(xmlDocPtr doc, xmlNodePtr node);
    bool
    readFileCaseChapter(xmlDocPtr doc, xmlNodePtr node);
    bool
    readFileCaseChapterDescription(xmlDocPtr doc, xmlNodePtr node);
    bool
    readFileCaseChapterIndex(xmlDocPtr doc, xmlNodePtr node);
    bool
    readFileCaseModuleName(xmlDocPtr doc, xmlNodePtr node);
    bool
    readFileCaseImage(xmlDocPtr doc, xmlNodePtr node);

    bool
    readFileCaseLink(xmlDocPtr doc, xmlNodePtr node, std::wstring& name, std::wstring& url);

    bool
    haveExample();
    bool
    copyHtmlDependencies();
    void
    setDestinationFile(std::wstring filenameDestination);

    void
    writeNavigatorBarAsHtml(std::string& utf8stream);
};
//=============================================================================
} // namespace Nelson
//=============================================================================
