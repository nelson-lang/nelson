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
#include "ModulesManager.hpp"
#include "Localization.hpp"
#include <filesystem>
#include <fstream>
#include <libxml/parser.h>
#include <libxml/xpathInternals.h>
#include <libxml/xmlerror.h>
//=============================================================================
namespace Nelson {
//=============================================================================
void
XmlPrettyPrint(const wstringVector& xmlFilesOrDirectories, std::wstring& errorMessage)
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
            errorMessage = L"File or directory does not exist: " + fileOrDir;
            return;
        }
    }
    for (const auto& xmlFile : xmlFiles) {
        std::ifstream file;
        std::string xmlFileUtf8 = wstring_to_utf8(xmlFile);

#ifdef _MSC_VER
        file.open(xmlFile, std::ios::binary);
#else
        file.open(xmlFileUtf8, std::ios::binary);
#endif
        if (!file) {
            errorMessage = L"Cannot open file: " + xmlFile;
            return;
        }
        std::string buffer(
            (std::istreambuf_iterator<char>(file)), std::istreambuf_iterator<char>());
        file.close();
        if (buffer.empty()) {
            errorMessage = L"File is empty or cannot be read: " + xmlFile;
            return;
        }
        // Parse the XML from memory
        xmlDocPtr doc = xmlReadMemory(buffer.data(), (int)buffer.size(), xmlFileUtf8.c_str(),
            nullptr, XML_PARSE_NOERROR | XML_PARSE_NOWARNING);
        if (!doc) {
            errorMessage = L"Failed to parse XML file: " + xmlFile;
            return;
        }

        // Dump the non-pretty XML to memory (format = 0)
        xmlChar* xmlbuff = nullptr;
        int buffersize = 0;
        xmlDocDumpFormatMemoryEnc(doc, &xmlbuff, &buffersize, "UTF-8", 0);
        if (xmlbuff) {
            xmlFree(xmlbuff);
            xmlbuff = nullptr;
        }
        // Dump the pretty XML to memory (format = 1)
        buffersize = 0;
        xmlDocDumpFormatMemoryEnc(doc, &xmlbuff, &buffersize, "UTF-8", 1);
        if (!xmlbuff) {
            xmlFreeDoc(doc);
            errorMessage = L"Failed to prettify XML file: " + xmlFile;
            return;
        }
// Write the formatted XML back to the file
#ifdef _MSC_VER
        std::ofstream outFile(xmlFile, std::ios::binary);
#else
        std::ofstream outFile(wstring_to_utf8(xmlFile), std::ios::binary);
#endif
        if (!outFile) {
            xmlFree(xmlbuff);
            xmlFreeDoc(doc);
            errorMessage = L"Cannot write to file: " + xmlFile;
            return;
        }
        outFile.write(reinterpret_cast<const char*>(xmlbuff), buffersize);
        outFile.close();
        xmlFree(xmlbuff);
        xmlFreeDoc(doc);
    }
}
//=============================================================================
void
XmlPrettyPrint(std::wstring& errorMessage)
{
    wstringVector modulesPaths = ModulesManager::Instance().getModulesPathList(false);
    wstringVector xmlPaths;
    wstringVector langs;
    Localization::Instance()->getManagedLanguages(langs);
    for (size_t i = 0; i < modulesPaths.size(); ++i) {
        for (size_t j = 0; j < langs.size(); ++j) {
            std::wstring xmlPath = modulesPaths[i] + L"/help/" + langs[j] + L"/xml";
            if (std::filesystem::is_directory(xmlPath)) {
                xmlPaths.push_back(xmlPath);
            }
        }
    }
    XmlPrettyPrint(xmlPaths, errorMessage);
}
//=============================================================================
}
//=============================================================================
