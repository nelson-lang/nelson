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
#include "ErrorID.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
// xmlFile, exampleContent, imageDestination
using xmlInfo = std::tuple<std::wstring, std::string, std::wstring>;
//=============================================================================
static bool
extractExamplesFromXmlDocFile(
    const std::wstring& xmlFilename, std::vector<xmlInfo>& infos, std::wstring& errorMessage);
static bool
imageNeedsGeneration(
    const FileSystemWrapper::Path& xmlFilePath, const FileSystemWrapper::Path& imageDestPath);
static std::wstring
scriptBaseNameForImage(const FileSystemWrapper::Path& imageDestPath);
//=============================================================================
bool
xmlDocGenerateImages(const std::wstring& xmlFileOrDir, const std::wstring& outputDir,
    wstringVector& outputScriptFilenames, std::wstring& errorMessage, bool force)
{
    errorMessage.clear();
    FileSystemWrapper::Path xmlFileOrDirPath(xmlFileOrDir);
    if (!(xmlFileOrDirPath.is_regular_file() || xmlFileOrDirPath.is_directory())) {
        errorMessage = resolveMessageTemplate(L"Nelson:help_tools:firstExistingXmlFile",
            L"First argument must be an existing XML file or directory.");
        return false;
    }
    FileSystemWrapper::Path outputDirPath(outputDir);
    if (!outputDirPath.is_directory()) {
        errorMessage = resolveMessageTemplate(L"Nelson:help_tools:secondExistingDirectory",
            L"Second argument must be an existing directory.");
        return false;
    }

    wstringVector xmlFiles;

    if (xmlFileOrDirPath.is_regular_file()) {
        xmlFiles.push_back(xmlFileOrDirPath.generic_wstring());
    } else {
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
    }
    if (xmlFiles.empty()) {
        errorMessage = Nelson::resolveMessageTemplate(
            L"Nelson:help_tools:xmlFilesFound", L"No XML files found.");
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
            continue;
        }
        infosAllFiles.insert(infosAllFiles.end(), infos.begin(), infos.end());
    }
    outputScriptFilenames.reserve(infosAllFiles.size());
    for (const auto& [xmlFile, exampleContent, imageDestination] : infosAllFiles) {
        FileSystemWrapper::Path xmlFilePath(xmlFile);
        FileSystemWrapper::Path imageDestPath
            = (xmlFilePath.parent_path() / imageDestination).lexically_normal();
        if (!force && !imageNeedsGeneration(xmlFilePath, imageDestPath)) {
            continue;
        }
        std::wstring scriptFilename = outputDirPath.generic_wstring() + L"/example_help_"
            + scriptBaseNameForImage(imageDestPath) + L".m";
        std::ofstream fileOutput;
#if _MSC_VER
        fileOutput.open(scriptFilename, std::ios::binary);
#else
        fileOutput.open(wstring_to_utf8(scriptFilename), std::ios::binary);
#endif
        if (!fileOutput.is_open()) {
            errorMessage = formatI18nMessage(
                Nelson::resolveMessageTemplate(
                    L"Nelson:help_tools:cannotReadFile", L"Cannot read file: %s"),
                scriptFilename);
            return false;
        }
        fileOutput << exampleContent;
        fileOutput << "\n";
        fileOutput << "imageFigure = [];\n";
        fileOutput << "if exist('f', 'var') && isgraphics(f) && ~isempty(get(f, 'Children'))\n";
        fileOutput << "  imageFigure = f;\n";
        fileOutput
            << "elseif exist('fig', 'var') && isgraphics(fig) && ~isempty(get(fig, 'Children'))\n";
        fileOutput << "  imageFigure = fig;\n";
        fileOutput << "else\n";
        fileOutput << "  imageFigure = gcf();\n";
        fileOutput << "end\n";
        fileOutput << "sleep(1);\n";
        fileOutput << "imageDestination = \"" << imageDestPath.generic_string() << "\";\n";
        fileOutput << "imageDestination = char(imageDestination);\n";
        fileOutput << "imageDestinationDirectory = fileparts(imageDestination);\n";
        fileOutput << "if ~isdir(imageDestinationDirectory)\n";
        fileOutput << "  mkdir(imageDestinationDirectory);\n";
        fileOutput << "end\n";
        fileOutput << "saveas(imageFigure, imageDestination);\n";
        fileOutput << "if ~isempty(imageFigure) && isgraphics(imageFigure)\n";
        fileOutput << "  close(imageFigure);\n";
        fileOutput << "end\n";
        fileOutput << "exit(0);\n";

        fileOutput.close();
        outputScriptFilenames.push_back(scriptFilename);
    }
    return true;
}
//=============================================================================
bool
imageNeedsGeneration(
    const FileSystemWrapper::Path& xmlFilePath, const FileSystemWrapper::Path& imageDestPath)
{
    if (!imageDestPath.is_regular_file()) {
        return true;
    }

    std::string errorMessage;
    uintmax_t imageSize = FileSystemWrapper::Path::file_size(imageDestPath, errorMessage);
    if (!errorMessage.empty() || imageSize == 0) {
        return true;
    }

    errorMessage.clear();
    std::time_t imageTime = FileSystemWrapper::Path::last_write_time(imageDestPath, errorMessage);
    if (!errorMessage.empty()) {
        return true;
    }

    errorMessage.clear();
    std::time_t xmlTime = FileSystemWrapper::Path::last_write_time(xmlFilePath, errorMessage);
    if (!errorMessage.empty()) {
        return true;
    }

    return imageTime < xmlTime;
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
        errorMessage
            = formatI18nMessage(Nelson::resolveMessageTemplate(
                                    L"Nelson:help_tools:cannotReadFile", L"Cannot read file: %s"),
                xmlFilename);
        return false;
    }

    std::string buffer((std::istreambuf_iterator<char>(file)), std::istreambuf_iterator<char>());
    if (buffer.empty()) {
        errorMessage
            = formatI18nMessage(Nelson::resolveMessageTemplate(
                                    L"Nelson:help_tools:cannotReadFile", L"Cannot read file: %s"),
                xmlFilename);
        return false;
    }
    xmlDocPtr doc
        = xmlReadMemory(buffer.data(), (int)buffer.size(), xmlFilenameUtf8.c_str(), nullptr, 0);
    if (!doc) {
        errorMessage
            = formatI18nMessage(resolveMessageTemplate(L"Nelson:help_tools:cannotParseXmlFile",
                                    L"Cannot parse XML file: %s"),
                xmlFilename);
        return false;
    }

    xmlXPathContextPtr xpathCtx = xmlXPathNewContext(doc);
    if (!xpathCtx) {
        xmlFreeDoc(doc);
        errorMessage = resolveMessageTemplate(
            L"Nelson:help_tools:cannotCreateXpathContext", L"Cannot create XPath context");
        return false;
    }

    const xmlChar* xpathExpr = BAD_CAST "//example_item_img[@generate='true']";
    xmlXPathObjectPtr xpathObj = xmlXPathEvalExpression(xpathExpr, xpathCtx);

    if (!xpathObj) {
        xmlXPathFreeContext(xpathCtx);
        xmlFreeDoc(doc);
        errorMessage
            = Nelson::resolveMessageTemplate(L"Nelson:help_tools:errorEvaluatingXpathExpression",
                L"Error evaluating XPath expression");
        return false;
    }

    if (!xmlXPathNodeSetIsEmpty(xpathObj->nodesetval)) {
        for (int i = 0; i < xpathObj->nodesetval->nodeNr; i++) {
            xmlNodePtr imgNode = xpathObj->nodesetval->nodeTab[i];
            xmlChar* src = xmlGetProp(imgNode, BAD_CAST "src");

            xmlNodePtr parentNode = imgNode->parent;
            if (parentNode) {
                xmlNodePtr dataNode = parentNode->children;
                while (dataNode) {
                    if (xmlStrcmp(dataNode->name, BAD_CAST "example_item_data") == 0) {
                        xmlChar* content = xmlNodeGetContent(dataNode);
                        if (content) {
                            xmlInfo info;

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
static bool
isScriptBaseNameCharacter(wchar_t c)
{
    return (c >= L'a' && c <= L'z') || (c >= L'A' && c <= L'Z') || (c >= L'0' && c <= L'9')
        || c == L'_';
}
//=============================================================================
static std::wstring
scriptBaseNameForImage(const FileSystemWrapper::Path& imageDestPath)
{
    FileSystemWrapper::Path parent = imageDestPath.parent_path();
    FileSystemWrapper::Path grandParent = parent.parent_path();
    std::wstring baseName;
    std::wstring grandParentName = grandParent.filename().wstring();
    if (!grandParentName.empty()) {
        baseName += grandParentName + L"_";
    }
    std::wstring parentName = parent.filename().wstring();
    if (!parentName.empty()) {
        baseName += parentName + L"_";
    }
    baseName += imageDestPath.stem().wstring();
    for (auto& c : baseName) {
        if (!isScriptBaseNameCharacter(c)) {
            c = L'_';
        }
    }
    return baseName;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
