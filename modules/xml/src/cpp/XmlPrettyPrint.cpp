//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "XmlPrettyPrint.hpp"
#include "characters_encoding.hpp"
#include "Localization.hpp"
#include "i18n.hpp"
#include <filesystem>
#include <fstream>
#include <string>
#include <libxml/parser.h>
#include <libxml/xpathInternals.h>
#include <libxml/xmlerror.h>
//=============================================================================
namespace Nelson {
//=============================================================================
static std::string
readXmlFile(const std::wstring& xmlFile, std::wstring& errorMessage)
{
    std::string xmlFileUtf8 = wstring_to_utf8(xmlFile);
    std::ifstream file;
#ifdef _MSC_VER
    file.open(xmlFile, std::ios::binary);
#else
    file.open(xmlFileUtf8, std::ios::binary);
#endif
    if (!file) {
        errorMessage = _W("Cannot open file: ") + xmlFile;
        return {};
    }
    std::string buffer((std::istreambuf_iterator<char>(file)), std::istreambuf_iterator<char>());
    file.close();
    if (buffer.empty()) {
        errorMessage = _W("File is empty or cannot be read: ") + xmlFile;
        return {};
    }
    return buffer;
}
//=============================================================================
static std::string
formatXmlString(const std::string& xmlContent, const std::string& xmlFileUtf8, bool formatSpace,
    std::wstring& errorMessage)
{
    // Remove all whitespace and carriage return characters
    std::string noWhitespace;
    noWhitespace.reserve(xmlContent.size());
    for (char c : xmlContent) {
        if (c != ' ' && c != '\n' && c != '\r' && c != '\t') {
            noWhitespace += c;
        }
    }
    noWhitespace += '\n';

    // Fix malformed XML declaration if present
    const std::string badDecl = "<?xmlversion=\"1.0\"encoding=\"UTF-8\"?>";
    const std::string goodDecl = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>";
    size_t declPos = noWhitespace.find(badDecl);
    if (declPos != std::string::npos) {
        noWhitespace.replace(declPos, badDecl.length(), goodDecl);
    }

    if (!formatSpace) {
        // If no formatting requested, return compacted XML
        return noWhitespace;
    }

    // If formatting requested, use libxml to prettify
    xmlDocPtr doc = xmlReadMemory(noWhitespace.data(), (int)noWhitespace.size(),
        xmlFileUtf8.c_str(), nullptr, XML_PARSE_NOERROR | XML_PARSE_NOWARNING);
    if (!doc) {
        errorMessage = _W("Failed to parse XML for formatting: ") + utf8_to_wstring(xmlFileUtf8);
        return {};
    }
    xmlChar* xmlbuff = nullptr;
    int buffersize = 0;
    int previous = xmlKeepBlanksDefault(0);
    xmlDocDumpFormatMemoryEnc(doc, &xmlbuff, &buffersize, nullptr, 1); // 1 = pretty print
    xmlKeepBlanksDefault(previous);
    if (!xmlbuff) {
        xmlFreeDoc(doc);
        errorMessage = _W("Failed to prettify XML file: ") + utf8_to_wstring(xmlFileUtf8);
        return {};
    }
    std::string formattedXml(reinterpret_cast<const char*>(xmlbuff), buffersize);
    xmlFree(xmlbuff);
    xmlFreeDoc(doc);
    return formattedXml;
}
//=============================================================================
static bool
saveString(const std::wstring& xmlFile, const std::string& content, std::wstring& errorMessage)
{
#ifdef _MSC_VER
    std::ofstream outFile(xmlFile, std::ios::binary);
#else
    std::ofstream outFile(wstring_to_utf8(xmlFile), std::ios::binary);
#endif
    if (!outFile) {
        errorMessage = _W("Cannot write to file: ") + xmlFile;
        return false;
    }
    outFile.write(content.data(), content.size());
    outFile.close();
    return true;
}
//=============================================================================
void
XmlPrettyPrint(
    const wstringVector& xmlFilesOrDirectories, bool formatSpace, std::wstring& errorMessage)
{
    errorMessage.clear();
    wstringVector xmlFiles;
    for (const auto& fileOrDir : xmlFilesOrDirectories) {
        if (std::filesystem::is_directory(fileOrDir)) {
            for (const auto& entry : std::filesystem::directory_iterator(fileOrDir)) {
                if (entry.is_regular_file() && entry.path().extension() == L".xml") {
                    xmlFiles.push_back(entry.path().wstring());
                }
            }
        } else if (std::filesystem::is_regular_file(fileOrDir)) {
            xmlFiles.push_back(fileOrDir);
        } else {
            errorMessage = _W("File or directory does not exist: ") + fileOrDir;
            return;
        }
    }
    for (const auto& xmlFile : xmlFiles) {
        std::string xmlContent = readXmlFile(xmlFile, errorMessage);
        if (!errorMessage.empty())
            return;

        std::string xmlFileUtf8 = wstring_to_utf8(xmlFile);
        std::string formattedXml
            = formatXmlString(xmlContent, xmlFileUtf8, formatSpace, errorMessage);
        if (!errorMessage.empty())
            return;

        if (!saveString(xmlFile, formattedXml, errorMessage))
            return;
    }
}
//=============================================================================
}
//=============================================================================
