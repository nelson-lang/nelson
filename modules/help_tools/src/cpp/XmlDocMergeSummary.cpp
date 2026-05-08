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
#include "XmlHelpers.hpp"
#include "characters_encoding.hpp"
#include "i18n.hpp"
#include <cctype>
#include <ctime>
#include <filesystem>
#include <fstream>
#include <libxml/parser.h>
#include <libxml/xmlerror.h>
#include <libxml/xmlsave.h>
#include <libxml/xpathInternals.h>
#include <string>
#include <vector>
//=============================================================================
namespace Nelson {
//=============================================================================
struct KeywordInfo
{
    std::string name;
    std::string link;
    std::string description;
};
//=============================================================================
struct SectionInfo
{
    std::string moduleName;
    std::string name;
    std::string chapterDescription;
    std::vector<KeywordInfo> keywords;
    std::vector<SectionInfo> children;
    std::string tocVisibility = "true";
    std::string link;
};
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
        keyword.name = (const char*)name;
        keyword.link = (const char*)link;
        keyword.description = (const char*)description;
        result = true;
    }

    if (name) {
        xmlFree(name);
    }
    if (link) {
        xmlFree(link);
    }
    if (description) {
        xmlFree(description);
    }

    return result;
}
//=============================================================================
static bool
ProcessSectionNode(xmlNodePtr sectionNode, const std::string& moduleValue, SectionInfo& sectionInfo)
{
    sectionInfo.moduleName = moduleValue;

    xmlChar* sectionName = xmlGetProp(sectionNode, BAD_CAST "name");
    if (!sectionName) {
        return false;
    }
    sectionInfo.name = (const char*)sectionName;
    xmlFree(sectionName);

    xmlChar* tocVisibility = xmlGetProp(sectionNode, BAD_CAST "toc_visibility");
    if (tocVisibility) {
        sectionInfo.tocVisibility = (const char*)tocVisibility;
        xmlFree(tocVisibility);
    }

    xmlChar* link = xmlGetProp(sectionNode, BAD_CAST "link");
    if (link) {
        sectionInfo.link = (const char*)link;
        xmlFree(link);
    }

    for (xmlNodePtr child = sectionNode->children; child; child = child->next) {
        if (child->type != XML_ELEMENT_NODE) {
            continue;
        }
        if (xmlStrcmp(child->name, BAD_CAST "chapter_description") == 0) {
            xmlBufferPtr buffer = xmlBufferCreate();
            if (buffer) {
                xmlNodeDump(buffer, child->doc, child, 1, 1);
                sectionInfo.chapterDescription = (const char*)xmlBufferContent(buffer);
                xmlBufferFree(buffer);
            }
        } else if (xmlStrcmp(child->name, BAD_CAST "keyword") == 0) {
            KeywordInfo keyword;
            if (ProcessKeywordNode(child, keyword)) {
                sectionInfo.keywords.push_back(std::move(keyword));
            }
        } else if (xmlStrcmp(child->name, BAD_CAST "section") == 0) {
            SectionInfo childSection;
            if (ProcessSectionNode(child, moduleValue, childSection)) {
                sectionInfo.children.push_back(std::move(childSection));
            }
        }
    }
    return true;
}
//=============================================================================
static bool
ProcessXmlFile(const std::wstring& filename, std::vector<SectionInfo>& sections,
    std::string& mainPage, std::wstring& errorMessage)
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

    if (mainPage.empty()) {
        const char* mainSummaryXPath = "//help_summary/main_summary";
        xmlXPathObjectPtr mainSummaryObj
            = xmlXPathEvalExpression(BAD_CAST mainSummaryXPath, xpathCtx);
        if (mainSummaryObj && mainSummaryObj->nodesetval
            && mainSummaryObj->nodesetval->nodeNr > 0) {
            xmlNodePtr mainSummaryNode = mainSummaryObj->nodesetval->nodeTab[0];
            xmlChar* linkValue = xmlGetProp(mainSummaryNode, BAD_CAST "link");
            if (linkValue) {
                mainPage = (const char*)linkValue;
                xmlFree(linkValue);
            }
        }
        if (mainSummaryObj) {
            xmlXPathFreeObject(mainSummaryObj);
        }
    }

    std::string moduleValue;
    const char* moduleXPath = "//help_summary/toc/module";
    xmlXPathObjectPtr moduleObj = xmlXPathEvalExpression(BAD_CAST moduleXPath, xpathCtx);
    if (moduleObj && moduleObj->nodesetval && moduleObj->nodesetval->nodeNr > 0) {
        xmlChar* moduleContent = xmlNodeGetContent(moduleObj->nodesetval->nodeTab[0]);
        if (moduleContent) {
            moduleValue = (const char*)moduleContent;
            xmlFree(moduleContent);
        }
    }
    if (moduleObj) {
        xmlXPathFreeObject(moduleObj);
    }

    const char* xpathExpr = "/help_summary/toc/section";
    xmlXPathObjectPtr xpathObj = xmlXPathEvalExpression(BAD_CAST xpathExpr, xpathCtx);
    if (xpathObj && xpathObj->nodesetval) {
        for (int i = 0; i < xpathObj->nodesetval->nodeNr; i++) {
            SectionInfo sectionInfo;
            if (ProcessSectionNode(xpathObj->nodesetval->nodeTab[i], moduleValue, sectionInfo)) {
                sections.push_back(std::move(sectionInfo));
            }
        }
    }

    if (xpathObj) {
        xmlXPathFreeObject(xpathObj);
    }
    xmlXPathFreeContext(xpathCtx);
    xmlFreeDoc(doc);
    return true;
}
//=============================================================================
static void
AppendSectionNode(xmlNodePtr tocNode, const SectionInfo& section)
{
    xmlNodePtr sectionNode = xmlNewChild(tocNode, nullptr, BAD_CAST "section", nullptr);
    xmlNewProp(sectionNode, BAD_CAST "name", BAD_CAST section.name.c_str());
    if (section.tocVisibility == "false") {
        xmlNewProp(sectionNode, BAD_CAST "toc_visibility", BAD_CAST section.tocVisibility.c_str());
    }
    if (!section.link.empty()) {
        xmlNewProp(sectionNode, BAD_CAST "link", BAD_CAST section.link.c_str());
    }

    if (!section.chapterDescription.empty()) {
        addRawXml(sectionNode, section.chapterDescription);
    }

    for (const auto& keyword : section.keywords) {
        xmlNodePtr keywordNode = xmlNewChild(sectionNode, nullptr, BAD_CAST "keyword", nullptr);
        xmlNewProp(keywordNode, BAD_CAST "name", BAD_CAST keyword.name.c_str());
        std::string fullLink = "./" + section.moduleName + "/" + keyword.link;
        xmlNewProp(keywordNode, BAD_CAST "link", BAD_CAST fullLink.c_str());
        xmlNewProp(keywordNode, BAD_CAST "description", BAD_CAST keyword.description.c_str());
    }

    for (const auto& child : section.children) {
        AppendSectionNode(sectionNode, child);
    }
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

    for (const auto& section : sections) {
        AppendSectionNode(tocNode, section);
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
static std::string
normalizeTextForJson(const std::string& input)
{
    std::string result;
    result.reserve(input.length());

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

    if (!result.empty() && result.front() == ' ') {
        result.erase(0, 1);
    }
    if (!result.empty() && result.back() == ' ') {
        result.pop_back();
    }

    return result;
}
//=============================================================================
static std::string
escapeJsonString(const std::string& input)
{
    std::string normalizedText = normalizeTextForJson(input);

    std::string output;
    output.reserve(normalizedText.length() * 2);

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
static void
WriteSectionJsonEntries(std::ofstream& outFile, const SectionInfo& section, bool& firstEntry)
{
    if (section.tocVisibility == "false" || !section.link.empty()) {
        return;
    }
    {
        if (!firstEntry) {
            outFile << ",\n";
        }
        firstEntry = false;

        std::string escapedSectionName = escapeJsonString(removeTags(section.name));
        std::string escapedChapterDesc = escapeJsonString(removeTags(section.chapterDescription));

        outFile << "  {\n"
                << "    \"title\": \"" << escapedSectionName << "\",\n"
                << "    \"url\": \""
                << "./" << section.moduleName << "/"
                << "index.html"
                << "\",\n"
                << "    \"path\": \"sections/" << section.moduleName << "\",\n"
                << "    \"content\": \"" << escapedChapterDesc << "\"\n"
                << "  }";
    }

    for (const auto& keyword : section.keywords) {
        if (!firstEntry) {
            outFile << ",\n";
        }
        firstEntry = false;
        std::string fullLink = "./" + section.moduleName + "/" + keyword.link;
        std::string escapedName = escapeJsonString(keyword.name);
        std::string escapedDesc = escapeJsonString(keyword.description);

        outFile << "  {\n"
                << "    \"title\": \"" << escapedName << "\",\n"
                << "    \"url\": \"" << fullLink << "\",\n"
                << "    \"path\": \"" << fullLink << "\",\n"
                << "    \"content\": \"" << escapedDesc << "\"\n"
                << "  }";
    }

    for (const auto& child : section.children) {
        WriteSectionJsonEntries(outFile, child, firstEntry);
    }
}
//=============================================================================
static bool
CreateIndexJson(const std::vector<SectionInfo>& sections, const std::filesystem::path& outputDir,
    std::wstring& errorMessage)
{
    std::filesystem::path outputPath = outputDir / "index.json";

    std::ofstream outFile(outputPath);
    if (!outFile.is_open()) {
        errorMessage = _W("Cannot create output JSON file: ") + outputPath.wstring();
        return false;
    }

    outFile << "[\n";

    bool firstEntry = true;
    for (const auto& section : sections) {
        WriteSectionJsonEntries(outFile, section, firstEntry);
    }

    outFile << "\n]";
    outFile.close();

    return true;
}
//=============================================================================
static void
WriteSectionJsEntries(std::ofstream& outFile, const SectionInfo& section, bool& firstEntry)
{
    if (section.tocVisibility == "false" || !section.link.empty()) {
        return;
    }
    {
        if (!firstEntry) {
            outFile << ",\n";
        }
        firstEntry = false;

        std::string escapedSectionName = escapeJsonString(removeTags(section.name));
        std::string escapedChapterDesc = escapeJsonString(removeTags(section.chapterDescription));

        outFile << "  {\n"
                << "    \"title\": \"" << escapedSectionName << "\",\n"
                << "    \"url\": \""
                << "./" << section.moduleName << "/"
                << "index.html"
                << "\",\n"
                << "    \"path\": \"sections/" << section.moduleName << "\",\n"
                << "    \"content\": \"" << escapedChapterDesc << "\"\n"
                << "  }";
    }

    for (const auto& keyword : section.keywords) {
        if (!firstEntry) {
            outFile << ",\n";
        }
        firstEntry = false;
        std::string fullLink = "./" + section.moduleName + "/" + keyword.link;
        std::string escapedName = escapeJsonString(keyword.name);
        std::string escapedDesc = escapeJsonString(keyword.description);

        outFile << "  {\n"
                << "    \"title\": \"" << escapedName << "\",\n"
                << "    \"url\": \"" << fullLink << "\",\n"
                << "    \"path\": \"" << fullLink << "\",\n"
                << "    \"content\": \"" << escapedDesc << "\"\n"
                << "  }";
    }

    for (const auto& child : section.children) {
        WriteSectionJsEntries(outFile, child, firstEntry);
    }
}
//=============================================================================
static bool
CreateIndexJs(const std::vector<SectionInfo>& sections, const std::filesystem::path& outputDir,
    std::wstring& errorMessage)
{
    std::filesystem::path outputPath = outputDir / "index.js";

    std::ofstream outFile(outputPath);
    if (!outFile.is_open()) {
        errorMessage = _W("Cannot create output JS file: ") + outputPath.wstring();
        return false;
    }

    outFile << "// index.js - Search index for Nelson help system\n"
            << "// Auto-generated on " << std::time(nullptr) << "\n\n"
            << "// Define the index in a globally accessible variable\n"
            << "window.searchIndex = [\n";

    bool firstEntry = true;
    for (const auto& section : sections) {
        WriteSectionJsEntries(outFile, section, firstEntry);
    }

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
    sections.reserve(filenames.size() * 4);
    std::string mainPage;

    for (const auto& filename : filenames) {
        if (!ProcessXmlFile(filename, sections, mainPage, errorMessage)) {
            return false;
        }
    }

    if (sections.empty()) {
        errorMessage = _W("No sections found in input files");
        return false;
    }

    std::filesystem::path outputPath
        = std::filesystem::path(destinationDirectory) / L"help_summary.xml";
    if (!CreateMergedXmlDocument(sections, mainPage, outputPath, errorMessage)) {
        return false;
    }

    if (!CreateIndexJs(sections, std::filesystem::path(destinationDirectory), errorMessage)) {
        return false;
    }

    if (!CreateIndexJson(sections, std::filesystem::path(destinationDirectory), errorMessage)) {
        return false;
    }
    return true;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
