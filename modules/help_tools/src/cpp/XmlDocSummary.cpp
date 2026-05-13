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
#include "Nelson_VERSION.h"
#include "StringHelpers.hpp"
#include "XmlHelpers.hpp"
#include "XmlTransform.hpp"
#include "characters_encoding.hpp"
#include "i18n.hpp"
#include <filesystem>
#include <fstream>
#include <libxml/parser.h>
#include <libxml/xmlerror.h>
#include <libxml/xpathInternals.h>
//=============================================================================
namespace Nelson {
//=============================================================================
static std::string
assembleTocSummary(const std::vector<XmlDocSection>& xmlDocFiles,
    DOCUMENT_OUTPUT outputDocumentType, std::wstring& errorMessage);
//=============================================================================
static void
appendSection(
    xmlNodePtr tocNode, const XmlDocSection& section, const std::wstring& destinationFileExtension);
//=============================================================================
bool
XmlDocTocSummary(const std::wstring& destinationDirectory, std::vector<XmlDocSection>& xmlDocFiles,
    const std::wstring& xsltTocFilename, const std::wstring& xsltSummaryFilename,
    DOCUMENT_OUTPUT outputDocumentType, std::wstring& errorMessage)
{
    errorMessage.clear();

    std::string tocContent = assembleTocSummary(xmlDocFiles, outputDocumentType, errorMessage);
    if (!errorMessage.empty()) {
        return false;
    }

    std::wstring outputXmlFile = destinationDirectory + L"/help_toc_summary.xml";
    std::wstring destinationTocFile;
    std::wstring destinationSummayFile;

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
                outputDocumentType, errorMessage, destinationDirectory)) {
            return false;
        }
    }
    if (!XmlTransform(outputXmlFile, xsltSummaryFilename, destinationSummayFile, true,
            outputDocumentType, errorMessage, destinationDirectory)) {
        return false;
    }
    if (outputDocumentType == DOCUMENT_OUTPUT::MARKDOWN) {
        std::filesystem::remove(outputXmlFile);
    }

    return true;
}
//=============================================================================
static std::wstring
getDestinationFileExtension(DOCUMENT_OUTPUT outputDocumentType)
{
    switch (outputDocumentType) {
    case DOCUMENT_OUTPUT::HTML_WEB:
        return L".html";
    case DOCUMENT_OUTPUT::MARKDOWN:
        return L".md";
    default:
        return L".html";
    }
}
//=============================================================================
static bool
hasVisibleContent(const XmlDocSection& section)
{
    return !section.pages.empty() || !section.children.empty();
}
//=============================================================================
static std::string
assembleTocSummary(const std::vector<XmlDocSection>& xmlDocFiles,
    DOCUMENT_OUTPUT outputDocumentType, std::wstring& errorMessage)
{
    std::wstring destinationFileExtension = getDestinationFileExtension(outputDocumentType);

    xmlDocPtr doc = xmlNewDoc(BAD_CAST "1.0");
    if (!doc) {
        errorMessage = _W("Cannot create help summary XML document.");
        return std::string();
    }

    xmlNodePtr rootNode = xmlNewNode(nullptr, BAD_CAST "help_summary");
    xmlDocSetRootElement(doc, rootNode);
    xmlNodePtr tocNode = xmlNewChild(rootNode, nullptr, BAD_CAST "toc", nullptr);

    bool moduleWritten = false;
    for (const auto& xmlDocFile : xmlDocFiles) {
        if (!moduleWritten) {
            xmlNewChild(tocNode, nullptr, BAD_CAST "module",
                BAD_CAST wstring_to_utf8(xmlDocFile.moduleName).c_str());
            moduleWritten = true;
        }
        if (hasVisibleContent(xmlDocFile)) {
            appendSection(tocNode, xmlDocFile, destinationFileExtension);
        }
    }

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
static std::wstring
outputRelativeFilename(const XmlDocPage& page, const std::wstring& destinationFileExtension)
{
    std::wstring filename = page.relativeFilename;
    StringHelpers::replace_last(filename, L".xml", destinationFileExtension);
    return filename;
}
//=============================================================================
static void
appendSection(xmlNodePtr parentNode, const XmlDocSection& section,
    const std::wstring& destinationFileExtension)
{
    xmlNodePtr sectionNode = xmlNewChild(parentNode, nullptr, BAD_CAST "section", nullptr);
    xmlNewProp(
        sectionNode, BAD_CAST "name", BAD_CAST wstring_to_utf8(section.chapterTitle).c_str());
    xmlNodePtr descriptionNode
        = xmlNewChild(sectionNode, nullptr, BAD_CAST "chapter_description", nullptr);
    addRawXml(descriptionNode, section.chapterDescription.c_str());

    for (const auto& page : section.pages) {
        std::wstring xmlFilename = outputRelativeFilename(page, destinationFileExtension);
        for (const auto& keyword : page.keywordAndAlias) {
            xmlNodePtr keywordNode = xmlNewChild(sectionNode, nullptr, BAD_CAST "keyword", nullptr);
            xmlNewProp(keywordNode, BAD_CAST "name", BAD_CAST keyword.c_str());
            xmlNewProp(keywordNode, BAD_CAST "link", BAD_CAST wstring_to_utf8(xmlFilename).c_str());
            xmlNewProp(keywordNode, BAD_CAST "description", BAD_CAST page.shortDescription.c_str());
        }
    }

    for (const auto& child : section.children) {
        if (hasVisibleContent(child)) {
            appendSection(sectionNode, child, destinationFileExtension);
        }
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
