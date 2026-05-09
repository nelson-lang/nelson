//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "XmlDocLinkChecker.hpp"
#include "XmlDocListOfFiles.hpp"
#include "characters_encoding.hpp"
#include <filesystem>
#include <fstream>
#include <libxml/parser.h>
#include <libxml/tree.h>
#include <set>
//=============================================================================
namespace Nelson {
//=============================================================================
static void
collectValidTargets(const XmlDocSection& section, std::set<std::wstring>& validTargets)
{
    for (const auto& page : section.pages) {
        std::filesystem::path pageStem = std::filesystem::path(page.relativeFilename);
        pageStem.replace_extension();
        std::wstring target = pageStem.generic_wstring();
        if (!target.empty()) {
            validTargets.insert(target);
            if (!section.moduleName.empty()) {
                validTargets.insert(L"${" + section.moduleName + L"}" + target);
                validTargets.insert(L"{" + section.moduleName + L"}" + target);
            }
        }

        for (const auto& keywordOrAlias : page.keywordAndAlias) {
            if (keywordOrAlias.empty()) {
                continue;
            }
            std::wstring aliasTarget = utf8_to_wstring(keywordOrAlias);
            validTargets.insert(aliasTarget);
            if (!section.moduleName.empty()) {
                validTargets.insert(L"${" + section.moduleName + L"}" + aliasTarget);
                validTargets.insert(L"{" + section.moduleName + L"}" + aliasTarget);
            }
        }
    }
    for (const auto& child : section.children) {
        collectValidTargets(child, validTargets);
    }
}
//=============================================================================
static void
collectAllXmlFiles(const XmlDocSection& section, wstringVector& xmlFiles)
{
    std::filesystem::path chapterFile = std::filesystem::path(section.directory) / L"chapter.xml";
    xmlFiles.push_back(chapterFile.generic_wstring());
    for (const auto& page : section.pages) {
        xmlFiles.push_back(page.absoluteFilename);
    }
    for (const auto& child : section.children) {
        collectAllXmlFiles(child, xmlFiles);
    }
}
//=============================================================================
static bool
readXmlFile(const std::wstring& xmlFilename, std::string& buffer, std::wstring& errorMessage)
{
    std::ifstream file;
    std::string xmlFilenameUtf8 = wstring_to_utf8(xmlFilename);
#if _MSC_VER
    file.open(xmlFilename, std::ios::binary);
#else
    file.open(xmlFilenameUtf8, std::ios::binary);
#endif
    if (!file.is_open()) {
        errorMessage = L"Cannot read file: " + xmlFilename;
        return false;
    }
    buffer.assign(std::istreambuf_iterator<char>(file), std::istreambuf_iterator<char>());
    if (buffer.empty()) {
        errorMessage = L"Cannot read file: " + xmlFilename;
        return false;
    }
    return true;
}
//=============================================================================
static void
collectLinkErrors(xmlNodePtr node, const std::wstring& xmlFilename,
    const std::set<std::wstring>& validTargets, wstringVector& errors)
{
    for (xmlNodePtr current = node; current; current = current->next) {
        if (current->type == XML_ELEMENT_NODE && xmlStrcmp(current->name, BAD_CAST "link") == 0) {
            xmlChar* attribute = xmlGetProp(current, BAD_CAST "linkend");
            if (attribute) {
                std::wstring linkend = utf8_to_wstring(reinterpret_cast<const char*>(attribute));
                if (!linkend.empty() && validTargets.find(linkend) == validTargets.end()) {
                    errors.push_back(L"Missing XML link in " + xmlFilename + L": " + linkend);
                }
                xmlFree(attribute);
            }
        }
        if (current->children) {
            collectLinkErrors(current->children, xmlFilename, validTargets, errors);
        }
    }
}
//=============================================================================
static bool
checkXmlFile(const std::wstring& xmlFilename, const std::set<std::wstring>& validTargets,
    wstringVector& errors, std::wstring& errorMessage)
{
    std::string buffer;
    if (!readXmlFile(xmlFilename, buffer, errorMessage)) {
        return false;
    }

    std::string xmlFilenameUtf8 = wstring_to_utf8(xmlFilename);
    xmlDocPtr doc = xmlReadMemory(
        buffer.data(), static_cast<int>(buffer.size()), xmlFilenameUtf8.c_str(), nullptr, 0);
    if (!doc) {
        errorMessage = L"Cannot parse XML file: " + xmlFilename;
        return false;
    }

    xmlNodePtr root = xmlDocGetRootElement(doc);
    collectLinkErrors(root, xmlFilename, validTargets, errors);
    xmlFreeDoc(doc);
    return true;
}
//=============================================================================
bool
xmldoclinkchecker(const wstringVector& xmlDirectories, const wstringVector& xmlFilesToCheck,
    wstringVector& errors, wstringVector& warnings, std::wstring& errorMessage)
{
    errors.clear();
    warnings.clear();
    errorMessage.clear();

    std::vector<XmlDocSection> xmlDocFilesList;
    std::string language;
    if (!xmlDocListOfFiles(xmlDirectories, xmlDocFilesList, language, errorMessage)) {
        return false;
    }
    (void)language;

    std::set<std::wstring> validTargets;
    wstringVector effectiveXmlFiles = xmlFilesToCheck;
    for (const auto& section : xmlDocFilesList) {
        collectValidTargets(section, validTargets);
        if (effectiveXmlFiles.empty()) {
            collectAllXmlFiles(section, effectiveXmlFiles);
        }
    }

    for (const auto& xmlFile : effectiveXmlFiles) {
        if (!checkXmlFile(xmlFile, validTargets, errors, errorMessage)) {
            return false;
        }
    }

    return errors.empty();
}
//=============================================================================
} // namespace Nelson
//=============================================================================