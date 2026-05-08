//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "XmlDocToc.hpp"
#include "StringHelpers.hpp"
#include "XmlTransform.hpp"
#include "characters_encoding.hpp"
#include <filesystem>
#include <fstream>
#include <libxml/parser.h>
#include <libxml/xmlerror.h>
#include <libxml/xpathInternals.h>
//=============================================================================
namespace Nelson {
//=============================================================================
static std::string
assembleToc(const std::vector<XmlDocSection>& xmlDocFiles, DOCUMENT_OUTPUT outputDocumentType);
//=============================================================================
static void
appendSection(xmlNodePtr rootNode, const XmlDocSection& section,
    const std::wstring& destinationFileExtension);
//=============================================================================
bool
XmlDocToc(const std::wstring& destinationDirectory, std::vector<XmlDocSection>& xmlDocFiles,
    const std::wstring& xsltFilename, DOCUMENT_OUTPUT outputDocumentType,
    std::wstring& errorMessage)
{
    errorMessage.clear();

    std::string tocContent = assembleToc(xmlDocFiles, outputDocumentType);

    std::wstring outputXmlFile = destinationDirectory + L"/table_of_contents.xml";
    std::wstring destinationFile = destinationDirectory + L"/table_of_contents.html";

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

    if (!XmlTransform(outputXmlFile, xsltFilename, destinationFile, true, outputDocumentType,
            errorMessage, destinationDirectory)) {
        return false;
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
assembleToc(const std::vector<XmlDocSection>& xmlDocFiles, DOCUMENT_OUTPUT outputDocumentType)
{
    std::wstring destinationFileExtension = getDestinationFileExtension(outputDocumentType);

    xmlDocPtr doc = xmlNewDoc(BAD_CAST "1.0");
    xmlNodePtr rootNode = xmlNewNode(nullptr, BAD_CAST "toc");
    xmlDocSetRootElement(doc, rootNode);

    for (const auto& xmlDocFile : xmlDocFiles) {
        if (hasVisibleContent(xmlDocFile)) {
            appendSection(rootNode, xmlDocFile, destinationFileExtension);
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

    for (const auto& page : section.pages) {
        std::wstring xmlFilename = outputRelativeFilename(page, destinationFileExtension);
        for (const auto& keyword : page.keywordAndAlias) {
            xmlNodePtr keywordNode = xmlNewChild(sectionNode, nullptr, BAD_CAST "keyword", nullptr);
            xmlNewProp(keywordNode, BAD_CAST "name", BAD_CAST keyword.c_str());
            xmlNewProp(keywordNode, BAD_CAST "link", BAD_CAST wstring_to_utf8(xmlFilename).c_str());
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
