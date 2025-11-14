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
    // Do not remove all whitespace from the raw XML content — that breaks required spaces.
    // Only normalize/fix a known malformed XML declaration if present.
    std::string buffer = xmlContent;
    const std::string badDecl = "<?xmlversion=\"1.0\"encoding=\"UTF-8\"?>";
    const std::string goodDecl = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>";
    size_t declPos = buffer.find(badDecl);
    if (declPos != std::string::npos) {
        buffer.replace(declPos, badDecl.length(), goodDecl);
    }

    if (!formatSpace) {
        std::string compact;
        compact.reserve(buffer.size());
        bool in_quote = false;
        char quote_char = 0;
        bool last_space = false;
        for (size_t i = 0; i < buffer.size(); ++i) {
            char c = buffer[i];
            if (c == '"' || c == '\'') {
                // toggle in-quote state; quotes themselves are preserved
                if (!in_quote) {
                    in_quote = true;
                    quote_char = c;
                } else if (quote_char == c) {
                    in_quote = false;
                    quote_char = 0;
                }
                compact += c;
                last_space = false;
                continue;
            }
            // normalize whitespace chars to space
            if (c == '\n' || c == '\r' || c == '\t')
                c = ' ';

            if (in_quote) {
                // inside quotes preserve spaces/newlines-as-space exactly
                compact += c;
                // do not update last_space because spaces inside quotes are significant
                continue;
            } else {
                if (c == ' ') {
                    // lookahead: if next non-space char is '<' or previous output char is '>',
                    // skip emitting the space (removes spaces between tags / after declaration)
                    size_t j = i + 1;
                    while (j < buffer.size()
                        && (buffer[j] == ' ' || buffer[j] == '\n' || buffer[j] == '\r'
                            || buffer[j] == '\t'))
                        ++j;
                    char nextNonSpace = (j < buffer.size()) ? buffer[j] : 0;
                    char prevOut = compact.empty() ? 0 : compact.back();
                    if (nextNonSpace == '<' || prevOut == '>') {
                        // skip this space
                        last_space = false;
                        continue;
                    }

                    if (!last_space) {
                        compact += ' ';
                        last_space = true;
                    }
                } else {
                    compact += c;
                    last_space = false;
                }
            }
        }
        // trim leading/trailing spaces
        size_t start = 0;
        while (start < compact.size() && compact[start] == ' ')
            ++start;
        size_t end = compact.size();
        while (end > start && compact[end - 1] == ' ')
            --end;
        return compact.substr(start, end - start);
    }

    // Use libxml2 to prettify — pass filename as base URI and explicit encoding.
    xmlDocPtr doc = xmlReadMemory(buffer.data(), (int)buffer.size(), xmlFileUtf8.c_str(), "UTF-8",
        XML_PARSE_NOERROR | XML_PARSE_NOWARNING | XML_PARSE_NOBLANKS);
    if (!doc) {
        errorMessage = _W("Failed to parse XML for formatting: ") + utf8_to_wstring(xmlFileUtf8);
        return {};
    }
    xmlChar* xmlbuff = nullptr;
    int buffersize = 0;
    int previous = xmlKeepBlanksDefault(0);
    // Ask libxml2 to output with UTF-8 encoding and pretty print (last parameter = 1).
    xmlDocDumpFormatMemoryEnc(doc, &xmlbuff, &buffersize, "UTF-8", 1);
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
