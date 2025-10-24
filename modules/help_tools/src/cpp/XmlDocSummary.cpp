//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "XmlDocSummary.hpp"
#include "XmlTransform.hpp"
#include <filesystem>
#include <libxml/parser.h>
#include <libxml/xpathInternals.h>
#include <libxml/xmlerror.h>
#include <fstream>
#include "characters_encoding.hpp"
#include "StringHelpers.hpp"
#include "i18n.hpp"
#include "Nelson_VERSION.h"
//=============================================================================
namespace Nelson {
//=============================================================================
static std::string
assembleTocSummary(const std::vector<XMLDOCFILES>& xmlDocFiles, DOCUMENT_OUTPUT outputDocumentType,
    std::wstring& errorMessage);
//=============================================================================
static void
addRawXml(xmlNodePtr parent, const std::string& rawxml);
//=============================================================================
bool
XmlDocTocSummary(const std::wstring& destinationDirectory, std::vector<XMLDOCFILES>& xmlDocFiles,
    const std::wstring& xsltTocFilename, const std::wstring& xsltSummaryFilename,
    DOCUMENT_OUTPUT outputDocumentType, std::wstring& errorMessage)
{
    errorMessage.clear();

    std::string tocContent = assembleTocSummary(xmlDocFiles, outputDocumentType, errorMessage);
    if (!errorMessage.empty()) {
        return false;
    }

    // Generate XML TOC as requested
    std::wstring outputXmlFile;
    std::wstring destinationTocFile;
    std::wstring destinationSummayFile;

    outputXmlFile = destinationDirectory + L"/help_toc_summary.xml";

    if (outputDocumentType == DOCUMENT_OUTPUT::MARKDOWN) {
        destinationTocFile = destinationDirectory + L"/SUMMARY.md";
        destinationSummayFile = destinationDirectory + L"/README.md";
    } else if (outputDocumentType == DOCUMENT_OUTPUT::HTML_WEB) {
        destinationSummayFile = destinationDirectory + L"/index.html";
    } else {
        destinationTocFile = destinationDirectory + L"/help_toc.html";
        destinationSummayFile = destinationDirectory + L"/help_summary.html";
    }
    std::ofstream out;

#ifdef _MSC_VER
    out.open(outputXmlFile, std::ios::out | std::ios::binary);
#else
    out.open(wstring_to_utf8(outputXmlFile), std::ios::out | std::ios::binary);
#endif

    if (!out.is_open()) {
        errorMessage = L"Failed to create TOC XML file.";
        return false;
    }

    out << tocContent;
    out.close();

    if (outputDocumentType != DOCUMENT_OUTPUT::HTML_WEB) {

        if (!XmlTransform(outputXmlFile, xsltTocFilename, destinationTocFile, true,
                outputDocumentType, errorMessage)) {
            return false;
        }
    }
    if (!XmlTransform(outputXmlFile, xsltSummaryFilename, destinationSummayFile, true,
            outputDocumentType, errorMessage)) {
        return false;
    }
    if (outputDocumentType == DOCUMENT_OUTPUT::MARKDOWN) {
        // std::filesystem::remove(outputXmlFile);
    }

    return true;
}
//=============================================================================
std::string
assembleTocSummary(const std::vector<XMLDOCFILES>& xmlDocFiles, DOCUMENT_OUTPUT outputDocumentType,
    std::wstring& errorMessage)
{
    std::wstring destinationFileExtension;
    switch (outputDocumentType) {
    case DOCUMENT_OUTPUT::HTML_WEB:

        destinationFileExtension = L".html";
        break;
    case DOCUMENT_OUTPUT::MARKDOWN:
        destinationFileExtension = L".md";
        break;
    default:
        destinationFileExtension = L".html";
        break;
    }

    xmlDocPtr doc = xmlNewDoc(BAD_CAST "1.0");

    xmlNodePtr root_node = xmlNewNode(nullptr, BAD_CAST "help_summary");
    xmlDocSetRootElement(doc, root_node);

    xmlNodePtr toc_node = xmlNewChild(root_node, nullptr, BAD_CAST "toc", nullptr);

    for (const auto& xmlDocFile : xmlDocFiles) {
        std::wstring moduleName = std::get<1>(xmlDocFile);
        xmlNodePtr modulename_node = xmlNewChild(
            toc_node, nullptr, BAD_CAST "module", BAD_CAST wstring_to_utf8(moduleName).c_str());
        std::wstring chapterTitle = std::get<2>(xmlDocFile);
        std::string chapterDescription = std::get<3>(xmlDocFile);
        std::vector<XMLDOCFILE> docFiles = std::get<4>(xmlDocFile);
        if (docFiles.empty())
            continue;

        xmlNodePtr section_node = xmlNewChild(toc_node, nullptr, BAD_CAST "section", nullptr);
        xmlNewProp(section_node, BAD_CAST "name", BAD_CAST wstring_to_utf8(chapterTitle).c_str());
        xmlNodePtr description_node
            = xmlNewChild(section_node, nullptr, BAD_CAST "chapter_description", nullptr);

        addRawXml(description_node, chapterDescription.c_str());

        for (const auto& doc : docFiles) {
            std::vector<std::string> keywordAndAlias = std::get<0>(doc);
            std::string keywordDescription = std::get<1>(doc);
            std::wstring xmlFilename = std::get<2>(doc);
            StringHelpers::replace_last(xmlFilename, L".xml", destinationFileExtension);
            for (const auto& keyword : keywordAndAlias) {
                xmlNodePtr keyword_node
                    = xmlNewChild(section_node, nullptr, BAD_CAST "keyword", nullptr);
                xmlNewProp(keyword_node, BAD_CAST "name", BAD_CAST keyword.c_str());
                xmlNewProp(
                    keyword_node, BAD_CAST "link", BAD_CAST wstring_to_utf8(xmlFilename).c_str());
                xmlNewProp(
                    keyword_node, BAD_CAST "description", BAD_CAST keywordDescription.c_str());
            }
        }
    }

    // Serialize XML to string
    xmlChar* xmlbuff = nullptr;
    int buffersize = 0;
    xmlDocDumpFormatMemoryEnc(doc, &xmlbuff, &buffersize, "UTF-8", 1);
    std::string result;
    if (xmlbuff) {
        result.assign(reinterpret_cast<char*>(xmlbuff), buffersize);
        xmlFree(xmlbuff);
    }
    xmlFreeDoc(doc);
    return result;
}
//=============================================================================
void
addRawXml(xmlNodePtr parent, const std::string& rawxml)
{
    xmlDocPtr doc = parent->doc;
    xmlNodePtr list = nullptr;

    int ret = xmlParseInNodeContext(parent, rawxml.c_str(), (int)rawxml.size(), 0, &list);

    if (ret == 0 && list) {
        for (xmlNodePtr cur = list; cur; cur = cur->next) {
            xmlAddChild(parent, xmlCopyNode(cur, 1));
        }
        xmlFreeNodeList(list);
    }
}
//=============================================================================
}
//=============================================================================
