//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#ifdef _MSC_VER
#define _CRT_SECURE_NO_WARNINGS
#endif
#include "XmlDocMergeSummary.hpp"
#include <libxml/parser.h>
#include <libxml/xpathInternals.h>
#include <libxml/xmlerror.h>
#include <libxml/xmlsave.h>
#include <filesystem>
#include <fstream>
#include <string>
#include <mutex>
#include <vector>
#include <ctime>
#include <cctype>
#include "characters_encoding.hpp"
#include "i18n.hpp"
#include "omp_for_loop.hpp"
#include "XmlHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
using KeywordInfo = std::tuple<std::string, std::string, std::string>;
using SectionInfo = std::tuple<std::string, std::string, std::string, std::vector<KeywordInfo>,
    std::string, std::string>;
//=============================================================================
static std::string
ReadFileToBuffer(const std::wstring& filename, std::wstring& errorMessage)
{
    std::string filenameUtf8 = wstring_to_utf8(filename);
    std::ifstream file;
#if _MSC_VER
    file.open(filename, std::ios::binary);
#else
    file.open(filenameUtf8, std::ios::binary);
#endif
    if (!file.is_open()) {
        errorMessage = _W("Cannot read file: ") + filename;
        return std::string();
    }

    std::string buffer((std::istreambuf_iterator<char>(file)), std::istreambuf_iterator<char>());
    file.close();

    if (buffer.empty()) {
        errorMessage = _W("Cannot read file: ") + filename;
    }
    return buffer;
}
//=============================================================================
static bool
ProcessKeywordNode(xmlNodePtr keywordNode, KeywordInfo& keyword)
{
    xmlChar* name = xmlGetProp(keywordNode, BAD_CAST "name");
    xmlChar* link = xmlGetProp(keywordNode, BAD_CAST "link");
    xmlChar* description = xmlGetProp(keywordNode, BAD_CAST "description");

    bool result = false;
    if (name && link && description) {
        keyword = std::make_tuple((const char*)name, (const char*)link, (const char*)description);
        result = true;
    }

    if (name)
        xmlFree(name);
    if (link)
        xmlFree(link);
    if (description)
        xmlFree(description);

    return result;
}

//=============================================================================
static bool
ProcessSectionNode(xmlNodePtr sectionNode, const std::string& moduleValue, SectionInfo& sectionInfo)
{
    std::get<0>(sectionInfo) = moduleValue;

    xmlChar* sectionName = xmlGetProp(sectionNode, BAD_CAST "name");
    if (!sectionName) {
        return false;
    }
    std::get<1>(sectionInfo) = (const char*)sectionName;
    xmlFree(sectionName);

    xmlChar* tocVisibility = xmlGetProp(sectionNode, BAD_CAST "toc_visibility");
    if (tocVisibility) {
        std::get<4>(sectionInfo) = (const char*)tocVisibility;
        xmlFree(tocVisibility);
    } else {
        std::get<4>(sectionInfo) = "true";
    }

    xmlChar* link = xmlGetProp(sectionNode, BAD_CAST "link");
    if (link) {
        std::get<5>(sectionInfo) = (const char*)link;
        xmlFree(link);
    } else {
        std::get<5>(sectionInfo) = "";
    }

    std::vector<KeywordInfo> keywords;
    keywords.reserve(32); // Pre-allocate for common size

    for (xmlNodePtr child = sectionNode->children; child; child = child->next) {
        if (xmlStrcmp(child->name, BAD_CAST "chapter_description") == 0) {
            xmlBufferPtr buffer = xmlBufferCreate();
            if (buffer) {
                xmlNodeDump(buffer, child->doc, child, 1, 1);
                std::get<2>(sectionInfo) = (const char*)xmlBufferContent(buffer);
                xmlBufferFree(buffer);
            }
        } else if (xmlStrcmp(child->name, BAD_CAST "keyword") == 0) {
            KeywordInfo keyword;
            if (ProcessKeywordNode(child, keyword)) {
                keywords.push_back(std::move(keyword));
            }
        }
    }

    std::get<3>(sectionInfo) = std::move(keywords);
    return true;
}

//=============================================================================
static bool
ProcessXmlFile(const std::wstring& filename, std::string& moduleValue,
    std::vector<SectionInfo>& sections, std::string& mainPage, std::mutex& sectionsMutex,
    std::wstring& errorMessage)
{
    std::string buffer = ReadFileToBuffer(filename, errorMessage);
    if (buffer.empty()) {
        return false;
    }

    std::string filenameUtf8 = wstring_to_utf8(filename);
    xmlDocPtr doc
        = xmlReadMemory(buffer.data(), (int)buffer.size(), filenameUtf8.c_str(), nullptr, 0);
    if (!doc) {
        errorMessage = _W("Cannot parse XML file: ") + filename;
        return false;
    }

    xmlNodePtr root = xmlDocGetRootElement(doc);
    if (!root) {
        xmlFreeDoc(doc);
        errorMessage = _W("Empty XML document: ") + filename;
        return false;
    }

    xmlXPathContextPtr xpathCtx = xmlXPathNewContext(doc);
    if (!xpathCtx) {
        xmlFreeDoc(doc);
        errorMessage = _W("Cannot create XPath context: ") + filename;
        return false;
    }

    std::string localMainPage;
    if (mainPage.empty()) {
        const char* mainSummaryXPath = "//help_summary/main_summary";
        xmlXPathObjectPtr mainSummaryObj
            = xmlXPathEvalExpression(BAD_CAST mainSummaryXPath, xpathCtx);
        if (mainSummaryObj && mainSummaryObj->nodesetval
            && mainSummaryObj->nodesetval->nodeNr > 0) {
            xmlNodePtr mainSummaryNode = mainSummaryObj->nodesetval->nodeTab[0];
            xmlChar* linkValue = xmlGetProp(mainSummaryNode, BAD_CAST "link");
            if (linkValue) {
                localMainPage = (const char*)linkValue;
                xmlFree(linkValue);
            }
            xmlXPathFreeObject(mainSummaryObj);
        }
    }

    std::string localModuleValue;
    const char* moduleXPath = "//help_summary/toc/module";
    xmlXPathObjectPtr moduleObj = xmlXPathEvalExpression(BAD_CAST moduleXPath, xpathCtx);
    if (moduleObj && moduleObj->nodesetval && moduleObj->nodesetval->nodeNr > 0) {
        xmlChar* moduleContent = xmlNodeGetContent(moduleObj->nodesetval->nodeTab[0]);
        if (moduleContent) {
            localModuleValue = (const char*)moduleContent;
            xmlFree(moduleContent);
        }
        xmlXPathFreeObject(moduleObj);
    }

    const char* xpathExpr = "//help_summary/toc/section";
    xmlXPathObjectPtr xpathObj = xmlXPathEvalExpression(BAD_CAST xpathExpr, xpathCtx);

    std::vector<SectionInfo> localSections;
    if (xpathObj && xpathObj->nodesetval) {
        localSections.reserve(xpathObj->nodesetval->nodeNr);
        for (int i = 0; i < xpathObj->nodesetval->nodeNr; i++) {
            xmlNodePtr sectionNode = xpathObj->nodesetval->nodeTab[i];
            SectionInfo sectionInfo;
            if (ProcessSectionNode(sectionNode, localModuleValue, sectionInfo)) {
                localSections.push_back(std::move(sectionInfo));
            }
        }
    }

    // Lock only when merging results
    {
        std::lock_guard<std::mutex> lock(sectionsMutex);
        moduleValue = localModuleValue;
        sections.insert(sections.end(), std::make_move_iterator(localSections.begin()),
            std::make_move_iterator(localSections.end()));
        if (!localMainPage.empty() && mainPage.empty()) {
            mainPage = localMainPage;
        }
    }

    xmlXPathFreeObject(xpathObj);
    xmlXPathFreeContext(xpathCtx);
    xmlFreeDoc(doc);
    return true;
}
//=============================================================================
static bool
CreateMergedXmlDocument(const std::vector<SectionInfo>& sections, const std::string& mainPage,
    const std::filesystem::path& outputPath, std::wstring& errorMessage)
{
    xmlDocPtr mergedDoc = xmlNewDoc(BAD_CAST "1.0");
    if (!mergedDoc) {
        errorMessage = _W("Cannot create output XML document");
        return false;
    }

    xmlNodePtr rootNode = xmlNewNode(nullptr, BAD_CAST "help_summary");
    if (!mainPage.empty()) {
        xmlNodePtr mainSummaryNode
            = xmlNewChild(rootNode, nullptr, BAD_CAST "main_summary", nullptr);
        xmlNewProp(mainSummaryNode, BAD_CAST "link", BAD_CAST mainPage.c_str());
    }
    xmlDocSetRootElement(mergedDoc, rootNode);
    xmlNodePtr tocNode = xmlNewChild(rootNode, nullptr, BAD_CAST "toc", nullptr);

    for (const auto& [moduleName, sectionName, chapterDesc, keywords, tocVisibility, link] :
        sections) {
        xmlNodePtr sectionNode = xmlNewChild(tocNode, nullptr, BAD_CAST "section", nullptr);
        xmlNewProp(sectionNode, BAD_CAST "name", BAD_CAST sectionName.c_str());
        if (tocVisibility == "false") {
            xmlNewProp(sectionNode, BAD_CAST "toc_visibility", BAD_CAST tocVisibility.c_str());
        }
        if (!link.empty()) {
            xmlNewProp(sectionNode, BAD_CAST "link", BAD_CAST link.c_str());
        }

        if (!chapterDesc.empty()) {
            addRawXml(sectionNode, chapterDesc);
        }

        for (const auto& [name, link, desc] : keywords) {
            xmlNodePtr keywordNode = xmlNewChild(sectionNode, nullptr, BAD_CAST "keyword", nullptr);
            xmlNewProp(keywordNode, BAD_CAST "name", BAD_CAST name.c_str());
            std::string fulllink = "./" + moduleName + "/" + link;
            xmlNewProp(keywordNode, BAD_CAST "link", BAD_CAST fulllink.c_str());
            xmlNewProp(keywordNode, BAD_CAST "description", BAD_CAST desc.c_str());
        }
    }

    std::string outputPathUtf8 = wstring_to_utf8(outputPath.wstring());
    xmlSaveCtxtPtr saveCtx
        = xmlSaveToFilename(outputPathUtf8.c_str(), "UTF-8", XML_SAVE_FORMAT | XML_SAVE_NO_EMPTY);
    if (!saveCtx) {
        xmlFreeDoc(mergedDoc);
        errorMessage = _W("Cannot create save context for: ") + outputPath.wstring();
        return false;
    }

    int res = xmlSaveDoc(saveCtx, mergedDoc);
    xmlSaveClose(saveCtx);
    xmlFreeDoc(mergedDoc);

    if (res < 0) {
        errorMessage = _W("Cannot save merged XML to: ") + outputPath.wstring();
        return false;
    }

    return true;
}

//=============================================================================
// Helper function to normalize and clean text for JSON
static std::string
normalizeTextForJson(const std::string& input)
{
    std::string result;
    result.reserve(input.length());

    // First pass: replace all whitespace sequences (spaces, tabs, newlines) with a single space
    bool prevWasSpace = false;
    for (char c : input) {
        if (std::isspace(static_cast<unsigned char>(c))) {
            if (!prevWasSpace) {
                result.push_back(' ');
                prevWasSpace = true;
            }
        } else {
            result.push_back(c);
            prevWasSpace = false;
        }
    }

    // Trim leading and trailing spaces
    if (!result.empty() && result.front() == ' ') {
        result.erase(0, 1);
    }
    if (!result.empty() && result.back() == ' ') {
        result.pop_back();
    }

    return result;
}

//=============================================================================
// Helper function to escape JSON strings
static std::string
escapeJsonString(const std::string& input)
{
    // First normalize the text to remove problematic whitespace
    std::string normalizedText = normalizeTextForJson(input);

    std::string output;
    output.reserve(normalizedText.length() * 2); // Reserve space for worst case

    for (char c : normalizedText) {
        switch (c) {
        case '\"':
            output += "\\\"";
            break;
        case '\\':
            output += "\\\\";
            break;
        case '/':
            output += "\\/";
            break;
        case '\b':
            output += "\\b";
            break;
        case '\f':
            output += "\\f";
            break;
        case '\n':
            output += "\\n";
            break;
        case '\r':
            output += "\\r";
            break;
        case '\t':
            output += "\\t";
            break;
        default:
            // Check for control characters (ASCII < 32)
            if (static_cast<unsigned char>(c) < 32) {
                char buf[8];
                sprintf(buf, "\\u%04x", static_cast<unsigned char>(c));
                output += buf;
            } else {
                output += c;
            }
        }
    }
    return output;
}
//=============================================================================
static std::string
removeTags(const std::string& input)
{
    std::string output;
    output.reserve(input.size());
    bool inTag = false;
    for (char c : input) {
        if (c == '<') {
            inTag = true;
        } else if (c == '>') {
            inTag = false;
        } else if (!inTag) {
            output += c;
        }
    }
    return output;
}
//=============================================================================
static bool
CreateIndexJson(const std::vector<SectionInfo>& sections, const std::filesystem::path& outputDir,
    std::wstring& errorMessage)
{
    std::filesystem::path outputPath = outputDir / "index.json";
    std::string outputPathUtf8 = wstring_to_utf8(outputPath.wstring());

    std::ofstream outFile(outputPath);
    if (!outFile.is_open()) {
        errorMessage = _W("Cannot create output JSON file: ") + outputPath.wstring();
        return false;
    }

    // Write JSON array start
    outFile << "[\n";

    // Process all sections and keywords
    bool firstEntry = true;
    for (const auto& [moduleName, sectionName, chapterDesc, keywords, tocVisibility, link] :
        sections) {
        if (tocVisibility == "false") {
            continue; // Skip sections not meant for TOC
        }
        if (!link.empty()) {
            continue; // Skip sections with custom links
        }
        // Add section entry
        if (!firstEntry) {
            outFile << ",\n";
        }
        firstEntry = false;

        // Process section name and description with proper JSON escaping
        std::string escapedSectionName = escapeJsonString(removeTags(sectionName));
        std::string escapedChapterDesc = escapeJsonString(removeTags(chapterDesc));

        outFile << "  {\n"
                << "    \"title\": \"" << escapedSectionName << "\",\n"
                << "    \"url\": \""
                << "/"
                << "./"
                << "index.html"
                << "\",\n"
                << "    \"path\": \"sections/" << moduleName << "\",\n"
                << "    \"content\": \"" << escapedChapterDesc << "\"\n"
                << "  }";

        // Add all keywords in this section
        for (const auto& [name, link, desc] : keywords) {
            outFile << ",\n";
            std::string fulllink = "./" + moduleName + "/" + link;

            // Escape keyword name and description
            std::string escapedName = escapeJsonString(name);
            std::string escapedDesc = escapeJsonString(desc);

            outFile << "  {\n"
                    << "    \"title\": \"" << escapedName << "\",\n"
                    << "    \"url\": \"" << fulllink << "\",\n"
                    << "    \"path\": \"" << fulllink << "\",\n"
                    << "    \"content\": \"" << escapedDesc << "\"\n"
                    << "  }";
        }
    }

    // Close the JSON array
    outFile << "\n]";
    outFile.close();

    return true;
}
//=============================================================================
static bool
CreateIndexJs(const std::vector<SectionInfo>& sections, const std::filesystem::path& outputDir,
    std::wstring& errorMessage)
{
    std::filesystem::path outputPath = outputDir / "index.js";
    std::string outputPathUtf8 = wstring_to_utf8(outputPath.wstring());

    std::ofstream outFile(outputPath);
    if (!outFile.is_open()) {
        errorMessage = _W("Cannot create output JS file: ") + outputPath.wstring();
        return false;
    }

    // Write header with timestamp to prevent caching issues
    outFile << "// index.js - Search index for Nelson help system\n"
            << "// Auto-generated on " << std::time(nullptr) << "\n\n"
            << "// Define the index in a globally accessible variable\n"
            << "window.searchIndex = [\n";

    // Process all sections and keywords
    bool firstEntry = true;
    for (const auto& [moduleName, sectionName, chapterDesc, keywords, tocVisibility, urlLink] :
        sections) {
        if (tocVisibility == "false") {
            continue; // Skip sections not meant for TOC
        }
        if (!urlLink.empty()) {
            continue; // Skip sections with custom links
        }
        // Add section entry
        if (!firstEntry) {
            outFile << ",\n";
        }
        firstEntry = false;

        // Process section name and description with proper JSON escaping
        std::string escapedSectionName = escapeJsonString(removeTags(sectionName));
        std::string escapedChapterDesc = escapeJsonString(removeTags(chapterDesc));

        outFile << "  {\n"
                << "    \"title\": \"" << escapedSectionName << "\",\n"
                << "    \"url\": \""
                << "./" << moduleName << "/"
                << "index.html"
                << "\",\n"
                << "    \"path\": \"sections/" << moduleName << "\",\n"
                << "    \"content\": \"" << escapedChapterDesc << "\"\n"
                << "  }";

        // Add all keywords in this section
        for (const auto& [name, link, desc] : keywords) {
            outFile << ",\n";
            std::string fulllink = "./" + moduleName + "/" + link;

            // Escape keyword name and description
            std::string escapedName = escapeJsonString(name);
            std::string escapedDesc = escapeJsonString(desc);

            outFile << "  {\n"
                    << "    \"title\": \"" << escapedName << "\",\n"
                    << "    \"url\": \"" << fulllink << "\",\n"
                    << "    \"path\": \"" << fulllink << "\",\n"
                    << "    \"content\": \"" << escapedDesc << "\"\n"
                    << "  }";
        }
    }

    // Close the array
    outFile << "\n];\n";
    outFile.close();

    return true;
}
//=============================================================================
bool
XmlDocMergeSummary(const std::wstring& destinationDirectory, const wstringVector& filenames,
    std::wstring& errorMessage)
{
    errorMessage.clear();

    std::vector<SectionInfo> sections;
    sections.reserve(filenames.size() * 4); // Pre-allocate with estimated size
    std::string moduleValue;

    std::vector<std::wstring> errorMessages(filenames.size());
    std::mutex sectionsMutex;
    std::string mainPage;

    OMP_PARALLEL_FOR_LOOP(filenames.size(), 8)
    for (int i = 0; i < filenames.size(); ++i) {
        std::wstring localError;
        if (!ProcessXmlFile(
                filenames[i], moduleValue, sections, mainPage, sectionsMutex, localError)) {
            errorMessages[i] = localError;
        }
    }

    // Check for errors after parallel processing
    for (const auto& error : errorMessages) {
        if (!error.empty()) {
            errorMessage = error;
            return false;
        }
    }

    if (sections.empty()) {
        errorMessage = _W("No sections found in input files");
        return false;
    }

    // Create help_summary.xml
    std::filesystem::path outputPath
        = std::filesystem::path(destinationDirectory) / L"help_summary.xml";
    if (!CreateMergedXmlDocument(sections, mainPage, outputPath, errorMessage)) {
        return false;
    }

    // Create index.js - primary search method
    if (!CreateIndexJs(sections, std::filesystem::path(destinationDirectory), errorMessage)) {
        return false;
    }

    // Create index.json for Nelson
    if (!CreateIndexJson(sections, std::filesystem::path(destinationDirectory), errorMessage)) {
        return false;
    }
    return true;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
