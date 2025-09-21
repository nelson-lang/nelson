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
#include "XmlTransform.hpp"
#include <filesystem>
#include <libxml/parser.h>
#include <libxml/xpathInternals.h>
#include <libxml/xmlerror.h>
#include <fstream>
#include "characters_encoding.hpp"
#include "StringHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static std::string
assembleToc(const std::vector<XMLDOCFILES>& xmlDocFiles, DOCUMENT_OUTPUT outputDocumentType);
//=============================================================================
bool
XmlDocToc(const std::wstring& destinationDirectory, std::vector<XMLDOCFILES>& xmlDocFiles,
    const std::wstring& xsltFilename, DOCUMENT_OUTPUT outputDocumentType,
    std::wstring& errorMessage)
{
    errorMessage.clear();

    std::string tocContent = assembleToc(xmlDocFiles, outputDocumentType);

    // Generate XML TOC as requested
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

    if (!XmlTransform(
            outputXmlFile, xsltFilename, destinationFile, true, outputDocumentType, errorMessage)) {
        return false;
    }
    return true;
}
//=============================================================================
std::string
assembleToc(const std::vector<XMLDOCFILES>& xmlDocFiles, DOCUMENT_OUTPUT outputDocumentType)
{
    std::wstring destinationFileExtension;
    switch (outputDocumentType) {
    case DOCUMENT_OUTPUT::HTML_LOCAL:
    case DOCUMENT_OUTPUT::HTML_WEB:
    case DOCUMENT_OUTPUT::QT_HELP:
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
    xmlNodePtr root_node = xmlNewNode(nullptr, BAD_CAST "toc");
    xmlDocSetRootElement(doc, root_node);

    for (const auto& xmlDocFile : xmlDocFiles) {
        std::wstring chapterTitle = std::get<2>(xmlDocFile);
        std::vector<XMLDOCFILE> docFiles = std::get<4>(xmlDocFile);
        if (docFiles.empty())
            continue;

        xmlNodePtr section_node = xmlNewChild(root_node, nullptr, BAD_CAST "section", nullptr);
        xmlNewProp(section_node, BAD_CAST "name", BAD_CAST wstring_to_utf8(chapterTitle).c_str());

        for (const auto& doc : docFiles) {
            std::vector<std::string> keywordAndAlias = std::get<0>(doc);
            std::string shortDescription = std::get<1>(doc);
            std::wstring xmlFilename = std::get<2>(doc);
            StringHelpers::replace_last(xmlFilename, L".xml", destinationFileExtension);
            for (const auto& keyword : keywordAndAlias) {
                xmlNodePtr keyword_node
                    = xmlNewChild(section_node, nullptr, BAD_CAST "keyword", nullptr);
                xmlNewProp(keyword_node, BAD_CAST "name", BAD_CAST keyword.c_str());
                xmlNewProp(
                    keyword_node, BAD_CAST "link", BAD_CAST wstring_to_utf8(xmlFilename).c_str());
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
}
//=============================================================================
