//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <filesystem>
#include <libxml/parser.h>
#include <libxml/xpathInternals.h>
#include <libxml/xmlerror.h>
#include <fstream>
#include <string>
#include <algorithm>
#include "XmlDocListOfFiles.hpp"
#include "characters_encoding.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static bool
readChapter(const std::wstring& directory, std::string& moduleName, std::string& chapterTitle,
    std::string& chapter_description, std::string& chapterLanguage, std::wstring& errorMessage);
//=============================================================================
static bool
readXmlDocFile(const std::wstring& xmlFilename, std::vector<std::string>& keywordAndAlias,
    std::string& shortDescription, std::wstring& errorMessage);
//=============================================================================
bool
xmlDocListOfFiles(const wstringVector& xmlDirectories, std::vector<XMLDOCFILES>& xmlDocFiles,
    std::string& language, std::wstring& errorMessage)
{
    language.clear();
    for (const auto& dir : xmlDirectories) {
        std::string moduleName;
        std::string chapterTitle;
        std::string chapterDescription;
        std::string chapterLanguage;
        if (!readChapter(
                dir, moduleName, chapterTitle, chapterDescription, chapterLanguage, errorMessage)) {
            return false;
        }
        if (!language.empty()) {
            if (language != chapterLanguage) {
                errorMessage = _W("Wrong language value in chapter.xml:") + dir;
                return false;
            }
        } else {
            language = chapterLanguage;
        }
        std::vector<XMLDOCFILE> xmlDocFilesInChapter;
        for (const auto& entry : std::filesystem::directory_iterator(dir)) {
            if (entry.is_regular_file()) {
                std::wstring path = entry.path().wstring();
                if (path.size() > 4) {
                    std::wstring ext = path.substr(path.size() - 4);
                    if (ext == L".xml" && entry.path().filename() != L"chapter.xml") {
                        std::vector<std::string> keywordAndAlias;
                        std::string short_description;
                        if (!readXmlDocFile(entry.path().generic_wstring(), keywordAndAlias,
                                short_description, errorMessage)) {
                            return false;
                        }
                        XMLDOCFILE xmlDocFile = std::make_tuple(
                            keywordAndAlias, short_description, entry.path().filename().wstring());
                        xmlDocFilesInChapter.push_back(xmlDocFile);
                    }
                }
            }
        }

        // Sort xmlDocFilesInChapter by filename (third element of tuple)
        std::sort(xmlDocFilesInChapter.begin(), xmlDocFilesInChapter.end(),
            [](const XMLDOCFILE& a, const XMLDOCFILE& b) {
                return std::get<2>(a) < std::get<2>(b);
            });

        xmlDocFiles.push_back(std::make_tuple(dir, utf8_to_wstring(moduleName),
            utf8_to_wstring(chapterTitle), chapterDescription, xmlDocFilesInChapter));
    }
    return true;
}
//=============================================================================
bool
readChapter(const std::wstring& directory, std::string& moduleName, std::string& chapterTitle,
    std::string& chapter_description, std::string& chapterLanguage, std::wstring& errorMessage)
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

    int count = 0;
    for (xmlNodePtr node = root ? root->children : nullptr; node; node = node->next) {
        if (node->type == XML_ELEMENT_NODE) {
            if (xmlStrcmp(node->name, BAD_CAST "language") == 0) {
                xmlChar* content = xmlNodeGetContent(node);
                if (content) {
                    chapterLanguage = (const char*)content;
                    xmlFree(content);
                    count++;
                }
            } else if (xmlStrcmp(node->name, BAD_CAST "chapter") == 0) {
                xmlChar* content = xmlNodeGetContent(node);
                if (content) {
                    chapterTitle = (const char*)content;
                    xmlFree(content);
                    count++;
                }
            } else if (xmlStrcmp(node->name, BAD_CAST "module_name") == 0) {
                xmlChar* content = xmlNodeGetContent(node);
                if (content) {
                    moduleName = (const char*)content;
                    xmlFree(content);
                    count++;
                }
            } else if (xmlStrcmp(node->name, BAD_CAST "chapter_description") == 0) {
                xmlBufferPtr buffer = xmlBufferCreate();
                if (buffer) {
                    for (xmlNodePtr child = node->children; child; child = child->next) {
                        xmlNodeDump(buffer, node->doc, child, 0, 0);
                    }
                    chapter_description = (const char*)xmlBufferContent(buffer);
                    xmlBufferFree(buffer);
                    count++;
                }
            }
        }
        if (count >= 4) {
            break;
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
    std::wstring keyword, keyword_alias;
    for (xmlNodePtr node = root->children; node; node = node->next) {
        if (node->type == XML_ELEMENT_NODE) {
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
                    break;
                }
            }
        }
    }
    xmlFreeDoc(doc);
    if (keywordAndAlias.empty() && shortDescription.empty()) {
        errorMessage = _W("No keyword and short_description tags.");
        return false;
    }
    return true;
}
//=============================================================================
}
//=============================================================================
