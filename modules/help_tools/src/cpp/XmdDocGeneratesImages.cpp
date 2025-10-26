//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <libxml/parser.h>
#include <libxml/xpathInternals.h>
#include <libxml/xmlerror.h>
#include <fstream>
#include <string>
#include <tuple>
#include <vector>
#include "XmlDocGenerateImages.hpp"
#include "FileSystemWrapper.hpp"
#include "i18n.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
// xmlFile, exampleContent, imageDestination
using xmlInfo = std::tuple<std::wstring, std::string, std::wstring>;
//=============================================================================
static bool
extractExamplesFromXmlDocFile(
    const std::wstring& xmlFilename, std::vector<xmlInfo>& infos, std::wstring& errorMessage);
//=============================================================================
bool
xmlDocGenerateImages(const std::wstring& xmlFileOrDir, const std::wstring& outputDir,
    wstringVector& outputScriptFilenames, std::wstring& errorMessage)
{
    errorMessage.clear();
    FileSystemWrapper::Path xmlFileOrDirPath(xmlFileOrDir);
    if (!(xmlFileOrDirPath.is_regular_file() || xmlFileOrDirPath.is_directory())) {
        errorMessage = _W("First argument must be an existing XML file or directory.");
        return false;
    }
    FileSystemWrapper::Path outputDirPath(outputDir);
    if (!outputDirPath.is_directory()) {
        errorMessage = _W("Second argument must be an existing directory.");
        return false;
    }

    wstringVector xmlFiles;

    if (xmlFileOrDirPath.is_regular_file()) {
        xmlFiles.push_back(xmlFileOrDirPath.generic_wstring());
    }
    for (const auto& entry : std::filesystem::directory_iterator(xmlFileOrDirPath.wstring())) {
        if (entry.is_regular_file()) {
            std::wstring path = entry.path().wstring();
            if (path.size() > 4) {
                std::wstring ext = path.substr(path.size() - 4);
                if (ext == L".xml") {
                    xmlFiles.push_back(entry.path().generic_wstring());
                }
            }
        }
    }
    if (xmlFiles.empty()) {
        errorMessage = _W("No XML files found.");
        return false;
    }

    std::vector<xmlInfo> infosAllFiles;

    for (const auto& xmlFile : xmlFiles) {
        wstringVector imageDestinations;
        std::vector<xmlInfo> infos;

        if (!extractExamplesFromXmlDocFile(xmlFile, infos, errorMessage)) {
            return false;
        }
        if (infos.empty()) {
            continue; // No examples to process
        }
        infosAllFiles.insert(infosAllFiles.end(), infos.begin(), infos.end());
    }
    outputScriptFilenames.reserve(infosAllFiles.size());
    for (const auto& [xmlFile, exampleContent, imageDestination] : infosAllFiles) {
        FileSystemWrapper::Path imageDestPath
            = FileSystemWrapper::Path(xmlFile).parent_path() / imageDestination;
        std::wstring scriptFilename = outputDirPath.generic_wstring() + L"/example_help_"
            + imageDestPath.stem().wstring() + L".m";
        std::ofstream fileOutput;
#if _MSC_VER
        fileOutput.open(scriptFilename, std::ios::binary);
#else
        fileOutput.open(wstring_to_utf8(scriptFilename), std::ios::binary);
#endif
        if (!fileOutput.is_open()) {
            errorMessage = _W("Cannot read file: ") + scriptFilename;
            return false;
        }
        fileOutput << exampleContent;
        fileOutput << "\n";
        fileOutput << "f = gcf();\n";
        fileOutput << "sleep(1);\n";
        fileOutput << "saveas(f, \"" << imageDestPath.generic_string() << "\");\n";
        fileOutput << "exit(0);\n";

        fileOutput.close();
        outputScriptFilenames.push_back(scriptFilename);
    }
    return true;
}
//=============================================================================
bool
extractExamplesFromXmlDocFile(
    const std::wstring& xmlFilename, std::vector<xmlInfo>& infos, std::wstring& errorMessage)
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

    xmlXPathContextPtr xpathCtx = xmlXPathNewContext(doc);
    if (!xpathCtx) {
        xmlFreeDoc(doc);
        errorMessage = _W("Cannot create XPath context");
        return false;
    }

    // Find all example_item_img elements with generate="true"
    const xmlChar* xpathExpr = BAD_CAST "//example_item_img[@generate='true']";
    xmlXPathObjectPtr xpathObj = xmlXPathEvalExpression(xpathExpr, xpathCtx);

    if (!xpathObj) {
        xmlXPathFreeContext(xpathCtx);
        xmlFreeDoc(doc);
        errorMessage = _W("Error evaluating XPath expression");
        return false;
    }

    if (!xmlXPathNodeSetIsEmpty(xpathObj->nodesetval)) {
        for (int i = 0; i < xpathObj->nodesetval->nodeNr; i++) {
            xmlNodePtr imgNode = xpathObj->nodesetval->nodeTab[i];
            xmlChar* src = xmlGetProp(imgNode, BAD_CAST "src");

            // Get the parent example_item node
            xmlNodePtr parentNode = imgNode->parent;
            if (parentNode) {
                // Find example_item_data node among siblings
                xmlNodePtr dataNode = parentNode->children;
                while (dataNode) {
                    if (xmlStrcmp(dataNode->name, BAD_CAST "example_item_data") == 0) {
                        xmlChar* content = xmlNodeGetContent(dataNode);
                        if (content) {
                            xmlInfo info;
                            // xmlFile, exampleContent, imageDestination

                            std::get<0>(info) = xmlFilename;
                            std::get<1>(info) = content ? (const char*)content : "";
                            std::get<2>(info) = src ? utf8_to_wstring((const char*)src) : L"";
                            infos.push_back(info);
                            xmlFree(content);
                        }
                        break;
                    }
                    dataNode = dataNode->next;
                }
            }

            if (src) {
                xmlFree(src);
            }
        }
    }

    xmlXPathFreeObject(xpathObj);
    xmlXPathFreeContext(xpathCtx);
    xmlFreeDoc(doc);
    return true;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
