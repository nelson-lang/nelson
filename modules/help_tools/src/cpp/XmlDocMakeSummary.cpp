//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "XmlDocMakeSummary.hpp"
#include "characters_encoding.hpp"
#include <libxml/parser.h>
#include <libxml/xpathInternals.h>
#include <libxml/xmlerror.h>
#include <filesystem>
#include <fstream>
#include <string>
//=============================================================================
namespace Nelson {
//=============================================================================
std::vector<std::tuple<std::wstring, std::wstring, std::wstring>>
XmlDocMakeSummary(const wstringVector xmlDirectories)
{
    std::vector<std::tuple<std::wstring, std::wstring, std::wstring>> summary_tuple;
    using namespace std::filesystem;
    for (const auto& dir : xmlDirectories) {
        path dirPath(dir);
        path chapterFile = dirPath / L"chapter.xml";
        std::wstring chapterName;
        if (exists(chapterFile)) {
            std::ifstream file;
            std::string chapterFileUtf8 = wstring_to_utf8(chapterFile.wstring());
#if _MSC_VER
            file.open(chapterFile.generic_wstring(), std::ios::binary);
#else
            file.open(chapterFileUtf8, std::ios::binary);
#endif
            std::string buffer(
                (std::istreambuf_iterator<char>(file)), std::istreambuf_iterator<char>());
            if (!buffer.empty()) {
                xmlDocPtr doc = xmlReadMemory(
                    buffer.data(), (int)buffer.size(), chapterFileUtf8.c_str(), nullptr, 0);
                if (doc) {
                    xmlNodePtr root = xmlDocGetRootElement(doc);
                    for (xmlNodePtr node = root->children; node; node = node->next) {
                        if (node->type == XML_ELEMENT_NODE
                            && xmlStrcmp(node->name, BAD_CAST "chapter") == 0) {
                            xmlChar* content = xmlNodeGetContent(node);
                            if (content) {
                                chapterName = utf8_to_wstring((const char*)content);
                                xmlFree(content);
                            }
                            break;
                        }
                    }
                    xmlFreeDoc(doc);
                }
            }
        }
        if (chapterName.empty())
            continue;
        for (const auto& entry : directory_iterator(dirPath)) {
            if (!entry.is_regular_file())
                continue;
            auto filePath = entry.path();
            if (filePath.extension() != L".xml")
                continue;
            if (filePath.filename() == L"chapter.xml")
                continue;
            std::string filePathUtf8 = wstring_to_utf8(filePath.wstring());
            std::ifstream file;
#if _MSC_VER
            file.open(filePath, std::ios::binary);
#else
            file.open(filePathUtf8, std::ios::binary);
#endif
            std::string buffer(
                (std::istreambuf_iterator<char>(file)), std::istreambuf_iterator<char>());
            if (buffer.empty())
                continue;
            xmlDocPtr doc = xmlReadMemory(
                buffer.data(), (int)buffer.size(), filePathUtf8.c_str(), nullptr, 0);
            if (!doc)
                continue;
            xmlNodePtr root = xmlDocGetRootElement(doc);
            std::wstring keyword, keyword_alias;
            for (xmlNodePtr node = root->children; node; node = node->next) {
                if (node->type == XML_ELEMENT_NODE) {
                    if (xmlStrcmp(node->name, BAD_CAST "keyword") == 0) {
                        xmlChar* content = xmlNodeGetContent(node);
                        if (content) {
                            keyword = utf8_to_wstring((const char*)content);
                            xmlFree(content);
                        }
                    } else if (xmlStrcmp(node->name, BAD_CAST "keyword_alias") == 0) {
                        xmlChar* content = xmlNodeGetContent(node);
                        if (content) {
                            keyword_alias = utf8_to_wstring((const char*)content);
                            xmlFree(content);
                        }
                    }
                }
            }
            if (!keyword.empty()) {
                summary_tuple.emplace_back(keyword, filePath.wstring(), chapterName);
            }
            if (!keyword_alias.empty()) {
                summary_tuple.emplace_back(keyword_alias, filePath.wstring(), chapterName);
            }
            xmlFreeDoc(doc);
        }
    }
    return summary_tuple;
}
//=============================================================================
}
//=============================================================================
