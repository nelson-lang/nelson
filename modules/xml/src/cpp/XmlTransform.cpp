//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#ifdef _MSC_VER
#define _CRT_SECURE_NO_WARNINGS
#endif
#include "XmlTransform.hpp"
#include "characters_encoding.hpp"
#include "XmlTarget.hpp"
#include "i18n.hpp"
#include <libxml/parser.h>
#include <libxml/xpathInternals.h>
#include <libxml/xmlerror.h>
#include <libxslt/xslt.h>
#include <libxslt/transform.h>
#include <libxslt/xsltutils.h>
#include <libxslt/extensions.h>
#include <filesystem>
#include <fstream>
#include <cstring>
#include <cstdarg>
#include <cstdlib>
#include <iterator>
#include <tuple>
#include <vector>
#include <mutex>
#include "ErrorID.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static std::string lastXSLTError;
static thread_local std::string copyImageError;
static thread_local std::string sourceCodeError;
static std::mutex xsltTransformMutex;
static bool
allowMissingHelpSources()
{
    const char* value = std::getenv("NELSON_HELP_ALLOW_MISSING_SOURCES");
    return value
        && (std::strcmp(value, "1") == 0 || std::strcmp(value, "TRUE") == 0
            || std::strcmp(value, "true") == 0);
}
//=============================================================================
void
xsltErrorCapture(void* /*ctx*/, const char* msg, ...)
{
    char buffer[1024];
    va_list args;
    va_start(args, msg);
    vsnprintf(buffer, sizeof(buffer), msg, args);
    va_end(args);
    lastXSLTError += buffer;
}
//=============================================================================
static void
ext_remove_module_prefix(xmlXPathParserContextPtr ctxt, int nargs)
{
    if (nargs != 1) {
        xmlXPathSetArityError(ctxt);
        return;
    }

    xmlChar* str = xmlXPathPopString(ctxt);
    if (str == nullptr) {
        xmlXPathReturnEmptyString(ctxt);
        return;
    }

    std::string input = reinterpret_cast<const char*>(str);
    xmlFree(str);
    std::string modifiedInput = input;

    if (input.rfind("${", 0) == 0) {
        size_t endModule = input.find('}', 2);
        if (endModule != std::string::npos) {
            std::string moduleName = input.substr(2, endModule - 2);
            std::string functionName = input.substr(endModule + 1);
            modifiedInput = "../" + moduleName + "/" + functionName + ".html";
        }
    } else {
        modifiedInput = input + ".html";
    }
    xmlChar* out = xmlStrdup(reinterpret_cast<const xmlChar*>(modifiedInput.c_str()));
    xmlXPathReturnString(ctxt, BAD_CAST out);
}
//=============================================================================
// Keeps the lines from the first one containing `start` to the first following
// one containing `end` (both inclusive). An empty `start` keeps the whole file.
static bool
extractSourceCodeRange(
    const std::string& content, const std::string& start, const std::string& end, std::string& out)
{
    if (start.empty()) {
        out = content;
        return true;
    }
    size_t startPos = content.find(start);
    if (startPos == std::string::npos) {
        return false;
    }
    size_t lineBegin = content.rfind('\n', startPos);
    lineBegin = (lineBegin == std::string::npos) ? 0 : lineBegin + 1;
    size_t lineEnd = content.size();
    if (!end.empty()) {
        size_t endPos = content.find(end, lineBegin);
        if (endPos == std::string::npos) {
            return false;
        }
        size_t newline = content.find('\n', endPos);
        if (newline != std::string::npos) {
            lineEnd = newline;
        }
    }
    out = content.substr(lineBegin, lineEnd - lineBegin);
    while (!out.empty() && (out.back() == '\n' || out.back() == '\r')) {
        out.pop_back();
    }
    return true;
}
//=============================================================================
static std::string
popOptionalString(xmlXPathParserContextPtr ctxt)
{
    xmlChar* value = xmlXPathPopString(ctxt);
    std::string result;
    if (value) {
        result = reinterpret_cast<const char*>(value);
        xmlFree(value);
    }
    return result;
}
//=============================================================================
static void
ext_source_code(xmlXPathParserContextPtr ctxt, int nargs)
{
    if (nargs != 1 && nargs != 3) {
        xmlXPathSetArityError(ctxt);
        return;
    }

    std::string rangeStart;
    std::string rangeEnd;
    if (nargs == 3) {
        rangeEnd = popOptionalString(ctxt);
        rangeStart = popOptionalString(ctxt);
    }

    xmlChar* pathAttr = xmlXPathPopString(ctxt);
    if (!pathAttr) {
        xmlXPathReturnEmptyString(ctxt);
        return;
    }

    std::string path = reinterpret_cast<const char*>(pathAttr);

    std::string xmlfilename = ".";
    if (ctxt && ctxt->context && ctxt->context->doc) {
        xmlChar* prop
            = xmlGetProp(xmlDocGetRootElement(ctxt->context->doc), BAD_CAST "xmlfilename");
        if (prop) {
            xmlfilename = reinterpret_cast<const char*>(prop);
            xmlFree(prop);
        }
    }

    std::filesystem::path sourcePath(path);
    if (sourcePath.is_relative()) {
        std::filesystem::path xmlPath(xmlfilename);
        std::filesystem::path candidate = xmlPath.parent_path() / sourcePath;
        std::error_code ec;
        if (!std::filesystem::is_regular_file(candidate, ec)) {
            for (auto parent = xmlPath.parent_path();
                !parent.empty() && parent != parent.parent_path(); parent = parent.parent_path()) {
                candidate = parent / sourcePath;
                if (std::filesystem::is_regular_file(candidate, ec)) {
                    sourcePath = candidate;
                    break;
                }
            }
        } else {
            sourcePath = candidate;
        }
    }

    std::error_code ec;
    if (!std::filesystem::is_regular_file(sourcePath, ec)) {
        if (allowMissingHelpSources()) {
            xmlXPathReturnEmptyString(ctxt);
            xmlFree(pathAttr);
            return;
        }
        sourceCodeError = "Cannot read help source code: source file does not exist: "
            + sourcePath.generic_string() + " referenced from " + xmlfilename;
        xmlXPathReturnEmptyString(ctxt);
        xmlFree(pathAttr);
        return;
    }

    std::ifstream input(sourcePath, std::ios::binary);
    if (!input) {
        if (allowMissingHelpSources()) {
            xmlXPathReturnEmptyString(ctxt);
            xmlFree(pathAttr);
            return;
        }
        sourceCodeError = "Cannot read help source code: " + sourcePath.generic_string();
        xmlXPathReturnEmptyString(ctxt);
        xmlFree(pathAttr);
        return;
    }

    std::string content((std::istreambuf_iterator<char>(input)), std::istreambuf_iterator<char>());
    std::string selected;
    if (!extractSourceCodeRange(content, rangeStart, rangeEnd, selected)) {
        if (allowMissingHelpSources()) {
            xmlXPathReturnEmptyString(ctxt);
            xmlFree(pathAttr);
            return;
        }
        sourceCodeError = "Cannot read help source code: range not found (start=\"" + rangeStart
            + "\", end=\"" + rangeEnd + "\") in " + sourcePath.generic_string()
            + " referenced from " + xmlfilename;
        xmlXPathReturnEmptyString(ctxt);
        xmlFree(pathAttr);
        return;
    }
    xmlXPathReturnString(ctxt, xmlStrdup(reinterpret_cast<const xmlChar*>(selected.c_str())));
    xmlFree(pathAttr);
}
//=============================================================================
static void
ext_copy_img(xmlXPathParserContextPtr ctxt, int nargs)
{
    if (nargs != 1 && nargs != 2) {
        xmlXPathSetArityError(ctxt);
        return;
    }

    bool allowMissingGeneratedImage = false;
    if (nargs == 2) {
        xmlChar* generateAttr = xmlXPathPopString(ctxt);
        if (generateAttr) {
            std::string generate = reinterpret_cast<const char*>(generateAttr);
            allowMissingGeneratedImage = (generate == "true");
            xmlFree(generateAttr);
        }
    }

    xmlChar* srcAttr = xmlXPathPopString(ctxt);
    if (!srcAttr) {
        xmlXPathReturnEmptyString(ctxt);
        return;
    }

    std::string src = reinterpret_cast<const char*>(srcAttr);

    std::string xmlfilename = ".";
    if (ctxt && ctxt->context && ctxt->context->doc) {
        xmlChar* prop
            = xmlGetProp(xmlDocGetRootElement(ctxt->context->doc), BAD_CAST "xmlfilename");
        if (prop) {
            xmlfilename = reinterpret_cast<const char*>(prop);
            xmlFree(prop);
        }
    }

    std::filesystem::path srcPath(src);
    if (srcPath.is_relative()) {
        srcPath = std::filesystem::path(xmlfilename).parent_path() / srcPath;
    }

    std::string outputDir = ".";
    if (ctxt && ctxt->context && ctxt->context->doc) {
        xmlChar* prop = xmlGetProp(xmlDocGetRootElement(ctxt->context->doc), BAD_CAST "outputdir");
        if (prop) {
            outputDir = reinterpret_cast<const char*>(prop);
            xmlFree(prop);
        }
    }

    std::string newFileName = srcPath.stem().string() + srcPath.extension().string();
    std::filesystem::path destPath = std::filesystem::path(outputDir) / newFileName;

    std::error_code ec;
    if (!std::filesystem::is_regular_file(srcPath, ec)) {
        if (allowMissingGeneratedImage || allowMissingHelpSources()) {
            xmlXPathReturnEmptyString(ctxt);
            xmlFree(srcAttr);
            return;
        }
        copyImageError = "Cannot copy help image: source file does not exist: "
            + srcPath.generic_string() + " referenced from " + xmlfilename;
        xmlXPathReturnEmptyString(ctxt);
        xmlFree(srcAttr);
        return;
    }

    std::filesystem::copy_file(
        srcPath, destPath, std::filesystem::copy_options::overwrite_existing, ec);
    if (ec) {
        copyImageError = "Cannot copy help image: " + srcPath.generic_string() + " to "
            + destPath.generic_string() + ": " + ec.message();
        xmlXPathReturnEmptyString(ctxt);
        xmlFree(srcAttr);
        return;
    }

    std::string newSrc = "./" + newFileName;
    xmlXPathReturnString(ctxt, xmlStrdup(reinterpret_cast<const xmlChar*>(newSrc.c_str())));
    xmlFree(srcAttr);
}
//=============================================================================
static std::string
getRelativePrefixToRoot(const std::wstring& outputfile, const std::wstring& rootOutputDirectory)
{
    if (rootOutputDirectory.empty()) {
        return std::string();
    }

    std::error_code ec;
    std::filesystem::path outputDirectory
        = std::filesystem::absolute(std::filesystem::path(outputfile).parent_path(), ec);
    if (ec) {
        return std::string();
    }
    std::filesystem::path rootDirectory
        = std::filesystem::absolute(std::filesystem::path(rootOutputDirectory), ec);
    if (ec) {
        return std::string();
    }

    std::filesystem::path relativeToRoot
        = std::filesystem::relative(rootDirectory, outputDirectory, ec);
    if (ec || relativeToRoot == L".") {
        return std::string();
    }
    std::string prefix = relativeToRoot.generic_string();
    if (!prefix.empty() && prefix.back() != '/') {
        prefix += '/';
    }
    return prefix;
}
//=============================================================================
// Adds "href", "target_module" and "target_page" attributes to every
// <link linkend="..."> element the resolver knows, so stylesheets do not have
// to guess the destination path.
static void
resolveLinkNodes(xmlNodePtr node, XmlLinkResolver* linkResolver, const std::string& rootPath,
    const std::wstring& xmlfile)
{
    for (xmlNodePtr current = node; current; current = current->next) {
        if (current->type == XML_ELEMENT_NODE && xmlStrcmp(current->name, BAD_CAST "link") == 0) {
            xmlChar* attribute = xmlGetProp(current, BAD_CAST "linkend");
            if (attribute) {
                std::string linkend = reinterpret_cast<const char*>(attribute);
                xmlFree(attribute);
                if (!linkend.empty()) {
                    XmlResolvedLink resolved;
                    if (linkResolver->resolve(linkend, rootPath, xmlfile, resolved)) {
                        xmlSetProp(current, BAD_CAST "href", BAD_CAST resolved.href.c_str());
                        xmlSetProp(current, BAD_CAST "target_module",
                            BAD_CAST resolved.targetModule.c_str());
                        xmlSetProp(
                            current, BAD_CAST "target_page", BAD_CAST resolved.targetPage.c_str());
                    } else {
                        linkResolver->unresolved(linkend, xmlfile);
                    }
                }
            }
        }
        if (current->children) {
            resolveLinkNodes(current->children, linkResolver, rootPath, xmlfile);
        }
    }
}
//=============================================================================
bool
XmlTransform(const std::wstring& xmlfile, const std::wstring& xslfile,
    const std::wstring& outputfile, bool overwrite, DOCUMENT_OUTPUT documentOutput,
    std::wstring& errorMessage, const std::wstring& rootOutputDirectory,
    const std::wstring& canonicalLink, const std::wstring& moduleName,
    XmlLinkResolver* linkResolver)
{
    static std::once_flag s_init_flag;
    std::call_once(s_init_flag, []() {
        xmlInitParser();
#if LIBXSLT_VERSION >= 10124
        // xsltInit is deprecated in newer libxslt versions but call if available
        xsltInit();
#endif
        xsltRegisterExtModuleFunction((const xmlChar*)"replace",
            (const xmlChar*)"http://io.github.nelson_lang/ext", Nelson::ext_remove_module_prefix);
        xsltRegisterExtModuleFunction((const xmlChar*)"copy_img",
            (const xmlChar*)"http://io.github.nelson_lang/ext", Nelson::ext_copy_img);
        xsltRegisterExtModuleFunction((const xmlChar*)"source_code",
            (const xmlChar*)"http://io.github.nelson_lang/ext", Nelson::ext_source_code);
    });

    std::lock_guard<std::mutex> transformLock(xsltTransformMutex);

    xsltStylesheetPtr style = nullptr;
    xmlDocPtr styledoc = xmlParseFile(wstring_to_utf8(xslfile).c_str());
    if (!styledoc) {
        errorMessage = formatI18nMessage(
            Nelson::resolveMessageTemplate(L"Nelson:xml:impossibleLoadXsltStylesheet",
                L"Impossible to load XSLT stylesheet: %s"),
            xslfile);
        return false;
    }
    style = xsltParseStylesheetDoc(styledoc);
    if (!style) {
        xmlFreeDoc(styledoc);
        errorMessage = formatI18nMessage(
            Nelson::resolveMessageTemplate(L"Nelson:xml:impossibleLoadXsltStylesheet",
                L"Impossible to load XSLT stylesheet: %s"),
            xslfile);
        return false;
    }

    errorMessage.clear();
    lastXSLTError.clear();
    copyImageError.clear();
    sourceCodeError.clear();
    if (!overwrite && std::filesystem::is_regular_file(outputfile)) {
        errorMessage = formatI18nMessage(
            Nelson::resolveMessageTemplate(
                L"Nelson:xml:destinationFileAlreadyExists", L"Destination file already exists: %s"),
            outputfile);
        if (style) {
            xsltFreeStylesheet(style);
        }
        return false;
    }
    xsltSetGenericErrorFunc(nullptr, xsltErrorCapture);

    xmlDocPtr doc = xmlParseFile(wstring_to_utf8(xmlfile).c_str());
    if (!doc) {
        errorMessage
            = formatI18nMessage(Nelson::resolveMessageTemplate(L"Nelson:xml:impossibleOpenParseXml",
                                    L"Impossible to open or parse XML file: %s"),
                xmlfile);
        if (style) {
            xsltFreeStylesheet(style);
        }
        return false;
    }

    std::string outputDir
        = std::filesystem::path(wstring_to_utf8(outputfile)).parent_path().string();
    if (outputDir.empty()) {
        outputDir = ".";
    }

    xmlSetProp(xmlDocGetRootElement(doc), BAD_CAST "outputdir", BAD_CAST outputDir.c_str());
    xmlSetProp(xmlDocGetRootElement(doc), BAD_CAST "xmlfilename",
        BAD_CAST wstring_to_utf8(xmlfile).c_str());
    std::string rootPath = getRelativePrefixToRoot(outputfile, rootOutputDirectory);
    xmlSetProp(xmlDocGetRootElement(doc), BAD_CAST "rootpath", BAD_CAST rootPath.c_str());
    xmlSetProp(xmlDocGetRootElement(doc), BAD_CAST "canonical_link",
        BAD_CAST wstring_to_utf8(canonicalLink).c_str());
    xmlSetProp(
        xmlDocGetRootElement(doc), BAD_CAST "module", BAD_CAST wstring_to_utf8(moduleName).c_str());
    if (linkResolver) {
        resolveLinkNodes(xmlDocGetRootElement(doc), linkResolver, rootPath, xmlfile);
    }

    xmlDocPtr result = xsltApplyStylesheet(style, doc, NULL);
    if (!result) {
        xmlFreeDoc(doc);
        if (style) {
            xsltFreeStylesheet(style);
            style = nullptr;
        }
        if (!lastXSLTError.empty()) {
            errorMessage += utf8_to_wstring(lastXSLTError);
        } else {
            errorMessage = Nelson::resolveMessageTemplate(
                L"Nelson:xml:errorApplyingXsltStylesheet", L"Error applying XSLT stylesheet.");
        }
        return false;
    }

    if (!copyImageError.empty()) {
        errorMessage = utf8_to_wstring(copyImageError);
        xmlFreeDoc(result);
        xmlFreeDoc(doc);
        if (style) {
            xsltFreeStylesheet(style);
            style = nullptr;
        }
        return false;
    }
    if (!sourceCodeError.empty()) {
        errorMessage = utf8_to_wstring(sourceCodeError);
        xmlFreeDoc(result);
        xmlFreeDoc(doc);
        if (style) {
            xsltFreeStylesheet(style);
            style = nullptr;
        }
        return false;
    }

    std::string outf8 = wstring_to_utf8(outputfile);
    int saveResult = xsltSaveResultToFilename(outf8.c_str(), result, style, 0);

    xmlFreeDoc(result);
    xmlFreeDoc(doc);
    if (style) {
        xsltFreeStylesheet(style);
        style = nullptr;
    }

    if (saveResult < 0) {
        errorMessage
            = formatI18nMessage(Nelson::resolveMessageTemplate(L"Nelson:xml:errorWritingOutputFile",
                                    L"Error writing output file: %s"),
                outputfile);
        return false;
    }

    return true;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
