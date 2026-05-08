//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "XmlDocListOfFiles.hpp"
#include "characters_encoding.hpp"
#include "i18n.hpp"
#include <algorithm>
#include <filesystem>
#include <fstream>
#include <libxml/parser.h>
#include <libxml/xmlerror.h>
#include <libxml/xpathInternals.h>
#include <string>
#include <vector>
//=============================================================================
namespace Nelson {
//=============================================================================
static bool
readChapter(const std::wstring& directory, std::string& moduleName, std::string& chapterTitle,
    std::string& chapterDescription, std::string& chapterLanguage, std::wstring& errorMessage);
//=============================================================================
static bool
readXmlDocFile(const std::wstring& xmlFilename, std::vector<std::string>& keywordAndAlias,
    std::string& shortDescription, std::wstring& errorMessage);
//=============================================================================
static bool
processDirectory(const std::filesystem::path& rootDirectory, const std::filesystem::path& directory,
    const std::string& inheritedModuleName, bool isRoot, XmlDocSection& section,
    std::string& language, std::wstring& errorMessage);
//=============================================================================
static bool
appendChildSections(const std::filesystem::path& rootDirectory,
    const std::filesystem::path& directory, const std::string& inheritedModuleName,
    std::vector<XmlDocSection>& children, std::string& language, std::wstring& errorMessage);
//=============================================================================
static bool
isXmlFile(const std::filesystem::path& path)
{
    return path.extension() == L".xml";
}
//=============================================================================
static std::vector<std::filesystem::directory_entry>
sortedDirectoryEntries(const std::filesystem::path& directory)
{
    std::vector<std::filesystem::directory_entry> entries;
    for (const auto& entry : std::filesystem::directory_iterator(directory)) {
        entries.push_back(entry);
    }
    std::sort(entries.begin(), entries.end(),
        [](const std::filesystem::directory_entry& a, const std::filesystem::directory_entry& b) {
            return a.path().generic_wstring() < b.path().generic_wstring();
        });
    return entries;
}
//=============================================================================
static std::wstring
relativeGenericWString(const std::filesystem::path& path, const std::filesystem::path& base)
{
    std::error_code ec;
    std::filesystem::path relativePath = std::filesystem::relative(path, base, ec);
    if (ec) {
        relativePath = path.filename();
    }
    if (relativePath == L".") {
        return L"";
    }
    return relativePath.generic_wstring();
}
//=============================================================================
static bool
directoryHasXmlFiles(const std::filesystem::path& directory)
{
    for (const auto& entry : std::filesystem::directory_iterator(directory)) {
        if (entry.is_regular_file() && isXmlFile(entry.path())) {
            return true;
        }
    }
    return false;
}
//=============================================================================
bool
xmlDocListOfFiles(const wstringVector& xmlDirectories, std::vector<XmlDocSection>& xmlDocFiles,
    std::string& language, std::wstring& errorMessage)
{
    language.clear();
    xmlDocFiles.clear();
    for (const auto& dir : xmlDirectories) {
        XmlDocSection section;
        std::filesystem::path rootDirectory(dir);
        if (!processDirectory(rootDirectory, rootDirectory, std::string(), true, section, language,
                errorMessage)) {
            return false;
        }
        xmlDocFiles.push_back(std::move(section));
    }
    return true;
}
//=============================================================================
static bool
processDirectory(const std::filesystem::path& rootDirectory, const std::filesystem::path& directory,
    const std::string& inheritedModuleName, bool isRoot, XmlDocSection& section,
    std::string& language, std::wstring& errorMessage)
{
    std::string moduleName;
    std::string chapterTitle;
    std::string chapterDescription;
    std::string chapterLanguage;
    if (!readChapter(directory.generic_wstring(), moduleName, chapterTitle, chapterDescription,
            chapterLanguage, errorMessage)) {
        return false;
    }
    if (chapterLanguage.empty()) {
        errorMessage = _W("No language value in chapter.xml: ") + directory.generic_wstring();
        return false;
    }
    if (!language.empty()) {
        if (language != chapterLanguage) {
            errorMessage
                = _W("Wrong language value in chapter.xml: ") + directory.generic_wstring();
            return false;
        }
    } else {
        language = chapterLanguage;
    }
    if (isRoot && moduleName.empty()) {
        errorMessage
            = _W("No module_name value in root chapter.xml: ") + directory.generic_wstring();
        return false;
    }
    if (!isRoot) {
        if (moduleName.empty()) {
            moduleName = inheritedModuleName;
        } else if (moduleName != inheritedModuleName) {
            errorMessage
                = _W("Wrong module_name value in chapter.xml: ") + directory.generic_wstring();
            return false;
        }
    }

    section.directory = directory.generic_wstring();
    section.relativeDirectory = relativeGenericWString(directory, rootDirectory);
    section.moduleName = utf8_to_wstring(moduleName);
    section.chapterTitle = utf8_to_wstring(chapterTitle);
    section.chapterDescription = chapterDescription;

    for (const auto& entry : sortedDirectoryEntries(directory)) {
        if (!entry.is_regular_file() || !isXmlFile(entry.path())) {
            continue;
        }
        if (entry.path().filename() == L"chapter.xml") {
            continue;
        }
        std::vector<std::string> keywordAndAlias;
        std::string shortDescription;
        if (!readXmlDocFile(
                entry.path().generic_wstring(), keywordAndAlias, shortDescription, errorMessage)) {
            return false;
        }
        XmlDocPage page;
        page.keywordAndAlias = std::move(keywordAndAlias);
        page.shortDescription = std::move(shortDescription);
        page.absoluteFilename = entry.path().generic_wstring();
        page.relativeFilename = relativeGenericWString(entry.path(), rootDirectory);
        section.pages.push_back(std::move(page));
    }

    std::sort(
        section.pages.begin(), section.pages.end(), [](const XmlDocPage& a, const XmlDocPage& b) {
            return a.relativeFilename < b.relativeFilename;
        });

    return appendChildSections(
        rootDirectory, directory, moduleName, section.children, language, errorMessage);
}
//=============================================================================
static bool
appendChildSections(const std::filesystem::path& rootDirectory,
    const std::filesystem::path& directory, const std::string& inheritedModuleName,
    std::vector<XmlDocSection>& children, std::string& language, std::wstring& errorMessage)
{
    for (const auto& entry : sortedDirectoryEntries(directory)) {
        if (!entry.is_directory()) {
            continue;
        }
        std::filesystem::path childDirectory = entry.path();
        std::filesystem::path childChapter = childDirectory / L"chapter.xml";
        if (std::filesystem::is_regular_file(childChapter)) {
            XmlDocSection child;
            if (!processDirectory(rootDirectory, childDirectory, inheritedModuleName, false, child,
                    language, errorMessage)) {
                return false;
            }
            children.push_back(std::move(child));
        } else {
            if (directoryHasXmlFiles(childDirectory)) {
                errorMessage = _W("No chapter.xml file in directory containing XML files: ")
                    + childDirectory.generic_wstring();
                return false;
            }
            if (!appendChildSections(rootDirectory, childDirectory, inheritedModuleName, children,
                    language, errorMessage)) {
                return false;
            }
        }
    }
    return true;
}
//=============================================================================
static bool
readChapter(const std::wstring& directory, std::string& moduleName, std::string& chapterTitle,
    std::string& chapterDescription, std::string& chapterLanguage, std::wstring& errorMessage)
{
    std::wstring chapterXml = directory + L"/chapter.xml";
    if (!std::filesystem::is_regular_file(chapterXml)) {
        errorMessage = _W("No chapter.xml file in directory: ") + directory;
        return false;
    }

    std::ifstream file;
    std::string chapterFileUtf8 = wstring_to_utf8(chapterXml);
#if _MSC_VER
    file.open(chapterXml, std::ios::binary);
#else
    file.open(chapterFileUtf8, std::ios::binary);
#endif
    if (!file.is_open()) {
        errorMessage = _W("Cannot read file: ") + chapterXml;
        return false;
    }
    std::string buffer((std::istreambuf_iterator<char>(file)), std::istreambuf_iterator<char>());
    if (buffer.empty()) {
        errorMessage = _W("Cannot read file: ") + chapterXml;
        return false;
    }
    xmlDocPtr doc
        = xmlReadMemory(buffer.data(), (int)buffer.size(), chapterFileUtf8.c_str(), nullptr, 0);
    if (!doc) {
        errorMessage = _W("Cannot parse XML file: ") + chapterXml;
        return false;
    }
    xmlNodePtr root = xmlDocGetRootElement(doc);

    for (xmlNodePtr node = root ? root->children : nullptr; node; node = node->next) {
        if (node->type != XML_ELEMENT_NODE) {
            continue;
        }
        if (xmlStrcmp(node->name, BAD_CAST "language") == 0) {
            xmlChar* content = xmlNodeGetContent(node);
            if (content) {
                chapterLanguage = (const char*)content;
                xmlFree(content);
            }
        } else if (xmlStrcmp(node->name, BAD_CAST "chapter") == 0) {
            xmlChar* content = xmlNodeGetContent(node);
            if (content) {
                chapterTitle = (const char*)content;
                xmlFree(content);
            }
        } else if (xmlStrcmp(node->name, BAD_CAST "module_name") == 0) {
            xmlChar* content = xmlNodeGetContent(node);
            if (content) {
                moduleName = (const char*)content;
                xmlFree(content);
            }
        } else if (xmlStrcmp(node->name, BAD_CAST "chapter_description") == 0) {
            xmlBufferPtr xmlBuffer = xmlBufferCreate();
            if (xmlBuffer) {
                for (xmlNodePtr child = node->children; child; child = child->next) {
                    xmlNodeDump(xmlBuffer, node->doc, child, 0, 0);
                }
                chapterDescription = (const char*)xmlBufferContent(xmlBuffer);
                xmlBufferFree(xmlBuffer);
            }
        }
    }
    xmlFreeDoc(doc);
    return true;
}
//=============================================================================
static bool
readXmlDocFile(const std::wstring& xmlFilename, std::vector<std::string>& keywordAndAlias,
    std::string& shortDescription, std::wstring& errorMessage)
{
    std::ifstream file;
    std::string xmlFilenameUtf8 = wstring_to_utf8(xmlFilename);
#if _MSC_VER
    file.open(xmlFilename, std::ios::binary);
#else
    file.open(xmlFilenameUtf8, std::ios::binary);
#endif
    if (!file.is_open()) {
        errorMessage = _W("Cannot read file: ") + xmlFilename;
        return false;
    }

    std::string buffer((std::istreambuf_iterator<char>(file)), std::istreambuf_iterator<char>());
    if (buffer.empty()) {
        errorMessage = _W("Cannot read file: ") + xmlFilename;
        return false;
    }
    xmlDocPtr doc
        = xmlReadMemory(buffer.data(), (int)buffer.size(), xmlFilenameUtf8.c_str(), nullptr, 0);
    if (!doc) {
        errorMessage = _W("Cannot parse XML file: ") + xmlFilename;
        return false;
    }

    xmlNodePtr root = xmlDocGetRootElement(doc);
    for (xmlNodePtr node = root ? root->children : nullptr; node; node = node->next) {
        if (node->type != XML_ELEMENT_NODE) {
            continue;
        }
        if (xmlStrcmp(node->name, BAD_CAST "keyword") == 0) {
            xmlChar* content = xmlNodeGetContent(node);
            if (content) {
                keywordAndAlias.push_back((const char*)content);
                xmlFree(content);
            }
        } else if (xmlStrcmp(node->name, BAD_CAST "keyword_alias") == 0) {
            xmlChar* content = xmlNodeGetContent(node);
            if (content) {
                keywordAndAlias.push_back((const char*)content);
                xmlFree(content);
            }
        } else if (xmlStrcmp(node->name, BAD_CAST "short_description") == 0) {
            xmlChar* content = xmlNodeGetContent(node);
            if (content) {
                shortDescription = (const char*)content;
                xmlFree(content);
            }
        }
    }
    xmlFreeDoc(doc);
    if (keywordAndAlias.empty() && shortDescription.empty()) {
        errorMessage = _W("No keyword and short_description tags: ") + xmlFilename;
        return false;
    }
    return true;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
