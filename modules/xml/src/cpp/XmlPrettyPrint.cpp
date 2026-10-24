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
#include <vector>
#include <libxml/parser.h>
#include <libxml/xpathInternals.h>
#include <libxml/xmlerror.h>
#include "ErrorID.hpp"
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
        errorMessage = formatI18nMessage(
            Nelson::resolveMessageTemplate(L"Nelson:xml:cannotOpenFile", L"Cannot open file: %s"),
            xmlFile);
        return {};
    }
    std::string buffer((std::istreambuf_iterator<char>(file)), std::istreambuf_iterator<char>());
    file.close();
    if (buffer.empty()) {
        errorMessage = formatI18nMessage(resolveMessageTemplate(L"Nelson:xml:fileEmptyCannotRead",
                                             L"File is empty or cannot be read: %s"),
            xmlFile);
        return {};
    }
    return buffer;
}
//=============================================================================
struct PreservedXmlBlock
{
    std::string placeholder;
    std::string original;
};
//=============================================================================
static void
preserveExampleItemDataBlocks(std::string& buffer, std::vector<PreservedXmlBlock>& preservedBlocks)
{
    const std::string startTag = "<example_item_data>";
    const std::string endTag = "</example_item_data>";
    size_t searchPos = 0;
    size_t counter = 0;
    while (true) {
        size_t start = buffer.find(startTag, searchPos);
        if (start == std::string::npos) {
            break;
        }
        size_t end = buffer.find(endTag, start);
        if (end == std::string::npos) {
            break;
        }
        end += endTag.size();
        PreservedXmlBlock block;
        block.placeholder = "<!--NELSON_XML_PRESERVE_" + std::to_string(counter++) + "-->";
        block.original = buffer.substr(start, end - start);
        buffer.replace(start, end - start, block.placeholder);
        preservedBlocks.push_back(std::move(block));
        searchPos = start + preservedBlocks.back().placeholder.size();
    }
}
//=============================================================================
static void
restoreExampleItemDataBlocks(
    std::string& buffer, const std::vector<PreservedXmlBlock>& preservedBlocks)
{
    for (const auto& block : preservedBlocks) {
        size_t pos = buffer.find(block.placeholder);
        if (pos != std::string::npos) {
            buffer.replace(pos, block.placeholder.size(), block.original);
        }
    }
}
//=============================================================================
static bool
startsWithOpeningBoldTag(const std::string& text, size_t pos)
{
    if (pos >= text.size() || text[pos] != '<') {
        return false;
    }
    return text.compare(pos, 3, "<b>") == 0 || text.compare(pos, 3, "<b ") == 0;
}
//=============================================================================
static bool
startsWithClosingBoldTag(const std::string& text, size_t pos)
{
    return pos + 4 <= text.size() && text.compare(pos, 4, "</b>") == 0;
}
//=============================================================================
static bool
startsWithBoldTag(const std::string& text, size_t pos)
{
    return startsWithOpeningBoldTag(text, pos) || startsWithClosingBoldTag(text, pos);
}
//=============================================================================
static bool
endsWithClosingBold(const std::string& text)
{
    return text.size() >= 4 && text.compare(text.size() - 4, 4, "</b>") == 0;
}
//=============================================================================
static std::string
formatXmlString(const std::string& xmlContent, const std::string& xmlFileUtf8, bool formatSpace,
    std::wstring& errorMessage)
{
    // Do not remove all whitespace from the raw XML content - that breaks required spaces.
    std::string buffer = xmlContent;
    const std::string badDecl = "<?xmlversion=\"1.0\"encoding=\"UTF-8\"?>";
    const std::string goodDecl = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>";
    size_t declPos = buffer.find(badDecl);
    if (declPos != std::string::npos) {
        buffer.replace(declPos, badDecl.length(), goodDecl);
    }

    std::vector<PreservedXmlBlock> preservedBlocks;
    if (formatSpace) {
        preserveExampleItemDataBlocks(buffer, preservedBlocks);
    }

    if (!formatSpace) {
        std::string compact;
        compact.reserve(buffer.size());
        bool in_quote = false;
        char quote_char = 0;
        bool last_space = false;
        const std::string exampleStartTag = "<example_item_data>";
        const std::string exampleEndTag = "</example_item_data>";
        const std::string cdataStart = "<![CDATA[";
        const std::string cdataEnd = "]]>";
        bool inExampleBlock = false;
        bool inCdataBlock = false;
        int boldDepth = 0;
        for (size_t i = 0; i < buffer.size(); ++i) {
            if (!inExampleBlock && !inCdataBlock
                && buffer.compare(i, exampleStartTag.size(), exampleStartTag) == 0) {
                inExampleBlock = true;
                compact.append(exampleStartTag);
                i += exampleStartTag.size() - 1;
                last_space = false;
                continue;
            }
            if (!inCdataBlock && buffer.compare(i, cdataStart.size(), cdataStart) == 0) {
                inCdataBlock = true;
                compact.append(cdataStart);
                i += cdataStart.size() - 1;
                last_space = false;
                continue;
            }
            if (inCdataBlock) {
                if (buffer.compare(i, cdataEnd.size(), cdataEnd) == 0) {
                    inCdataBlock = false;
                    compact.append(cdataEnd);
                    i += cdataEnd.size() - 1;
                } else {
                    compact += buffer[i];
                }
                continue;
            }
            if (inExampleBlock) {
                if (buffer.compare(i, exampleEndTag.size(), exampleEndTag) == 0) {
                    inExampleBlock = false;
                    compact.append(exampleEndTag);
                    i += exampleEndTag.size() - 1;
                } else {
                    compact += buffer[i];
                }
                continue;
            }
            if (!inExampleBlock && !inCdataBlock && buffer[i] == '<') {
                if (startsWithOpeningBoldTag(buffer, i)) {
                    ++boldDepth;
                } else if (startsWithClosingBoldTag(buffer, i) && boldDepth > 0) {
                    --boldDepth;
                }
            }
            char c = buffer[i];
            if (boldDepth > 0 && c == ' ') {
                compact += ' ';
                last_space = false;
                continue;
            }
            if (c == '"' || c == '\'') {
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
            if (c == '\n' || c == '\r' || c == '\t') {
                c = ' ';
            }

            if (in_quote) {
                // spaces inside quotes are significant: keep them, leave last_space alone
                compact += c;
                continue;
            } else {
                if (c == ' ') {
                    size_t j = i + 1;
                    while (j < buffer.size()
                        && (buffer[j] == ' ' || buffer[j] == '\n' || buffer[j] == '\r'
                            || buffer[j] == '\t')) {
                        ++j;
                    }
                    bool nextStartsBoldTag = startsWithBoldTag(buffer, j);
                    if (nextStartsBoldTag) {
                        compact += ' ';
                        last_space = true;
                        continue;
                    }
                    if (compact.size() >= 4 && compact.compare(compact.size() - 4, 4, "</b>") == 0
                        && j < buffer.size() && startsWithOpeningBoldTag(buffer, j)) {
                        compact += ' ';
                        last_space = true;
                        continue;
                    }
                    if (j < buffer.size() && buffer[j] == '<') {
                        compact += ' ';
                        last_space = true;
                        continue;
                    }
                    char nextNonSpace = (j < buffer.size()) ? buffer[j] : 0;
                    char prevOut = compact.empty() ? 0 : compact.back();
                    bool prevEndsClosingBold = endsWithClosingBold(compact);
                    if ((nextNonSpace == '<' && !nextStartsBoldTag)
                        || (prevOut == '>' && !prevEndsClosingBold)) {
                        last_space = false;
                        continue;
                    }
                    if (!last_space) {
                        compact += ' ';
                        last_space = true;
                    }
                } else {
                    if (c == '>' && i + 1 < buffer.size()) {
                        size_t k = i + 1;
                        while (k < buffer.size()
                            && (buffer[k] == ' ' || buffer[k] == '\n' || buffer[k] == '\r'
                                || buffer[k] == '\t')) {
                            ++k;
                        }
                        if (k < buffer.size() && buffer[k] == '<') {
                            compact += '>';
                            i = k - 1;
                            continue;
                        }
                    }
                    if (compact.size() >= 4 && compact.compare(compact.size() - 4, 4, "</b>") == 0
                        && startsWithOpeningBoldTag(buffer, i)) {
                        if (compact.back() != ' ') {
                            compact += ' ';
                        }
                    }
                    compact += c;
                    last_space = false;
                }
            }
        }
        size_t start = 0;
        while (start < compact.size() && compact[start] == ' ') {
            ++start;
        }
        size_t end = compact.size();
        while (end > start && compact[end - 1] == ' ') {
            --end;
        }
        return compact.substr(start, end - start);
    }

    xmlDocPtr doc = xmlReadMemory(buffer.data(), (int)buffer.size(), xmlFileUtf8.c_str(), "UTF-8",
        XML_PARSE_NOERROR | XML_PARSE_NOWARNING | XML_PARSE_NOBLANKS);
    if (!doc) {
        errorMessage
            = formatI18nMessage(resolveMessageTemplate(L"Nelson:xml:failedParseXmlFormatting",
                                    L"Failed to parse XML for formatting: %s"),
                utf8_to_wstring(xmlFileUtf8));
        return {};
    }
    xmlChar* xmlbuff = nullptr;
    int buffersize = 0;
    int previous = xmlKeepBlanksDefault(0);
    xmlDocDumpFormatMemoryEnc(doc, &xmlbuff, &buffersize, "UTF-8", 1);
    xmlKeepBlanksDefault(previous);
    if (!xmlbuff) {
        xmlFreeDoc(doc);
        errorMessage = formatI18nMessage(resolveMessageTemplate(L"Nelson:xml:failedPrettifyXmlFile",
                                             L"Failed to prettify XML file: %s"),
            utf8_to_wstring(xmlFileUtf8));
        return {};
    }
    std::string formattedXml(reinterpret_cast<const char*>(xmlbuff), buffersize);
    xmlFree(xmlbuff);
    xmlFreeDoc(doc);
    restoreExampleItemDataBlocks(formattedXml, preservedBlocks);
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
        errorMessage = formatI18nMessage(
            resolveMessageTemplate(L"Nelson:xml:cannotWriteFile", L"Cannot write to file: %s"),
            xmlFile);
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
            errorMessage
                = formatI18nMessage(resolveMessageTemplate(L"Nelson:xml:fileDirectoryDoesExist",
                                        L"File or directory does not exist: %s"),
                    fileOrDir);
            return;
        }
    }
    for (const auto& xmlFile : xmlFiles) {
        std::string xmlContent = readXmlFile(xmlFile, errorMessage);
        if (!errorMessage.empty()) {
            return;
        }

        std::string xmlFileUtf8 = wstring_to_utf8(xmlFile);
        std::string formattedXml
            = formatXmlString(xmlContent, xmlFileUtf8, formatSpace, errorMessage);
        if (!errorMessage.empty()) {
            return;
        }

        if (!saveString(xmlFile, formattedXml, errorMessage)) {
            return;
        }
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
