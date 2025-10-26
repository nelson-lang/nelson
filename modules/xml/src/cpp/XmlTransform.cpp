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
#include <iostream>
#include <filesystem>
#include <fstream>
#include <cstring>
#include <cstdarg>
#include <tuple>
#include <vector>
#include <regex>
#include <mutex>
//=============================================================================
namespace Nelson {
//=============================================================================
std::string lastXSLTError;
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

    try {
        // Capture ${content}
        std::regex re(R"(^\$\{([^}]+)\})");
        std::smatch match;
        if (std::regex_search(input, match, re)) {
            // match[1] is the word inside ${...}
            std::string content = match[1].str();
            // Replace with ../word/
            modifiedInput = std::regex_replace(input, re, "../" + content + "/") + ".html";
        } else {
            modifiedInput = input + ".html"; // fallback
        }
    } catch (const std::exception&) {
    }
    xmlChar* out = xmlStrdup(reinterpret_cast<const xmlChar*>(modifiedInput.c_str()));
    xmlXPathReturnString(ctxt, BAD_CAST out);
}
//=============================================================================
static void
ext_copy_img(xmlXPathParserContextPtr ctxt, int nargs)
{
    if (nargs != 1) {
        xmlXPathSetArityError(ctxt);
        return;
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

    try {
        std::filesystem::copy_file(
            srcPath, destPath, std::filesystem::copy_options::overwrite_existing);
    } catch (const std::exception&) {
    }
    std::string newSrc = "./" + newFileName;
    xmlXPathReturnString(ctxt, xmlStrdup(reinterpret_cast<const xmlChar*>(newSrc.c_str())));
    xmlFree(srcAttr);
}
//=============================================================================
bool
XmlTransform(const std::wstring& xmlfile, const std::wstring& xslfile,
    const std::wstring& outputfile, bool overwrite, DOCUMENT_OUTPUT documentOutput,
    std::wstring& errorMessage)
{
    // Initialize libxml/libxslt once and register extension functions once
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
    });

    xsltStylesheetPtr style = nullptr;
    xmlDocPtr styledoc = xmlParseFile(wstring_to_utf8(xslfile).c_str());
    if (!styledoc) {
        errorMessage = _W("Impossible to load XSLT stylesheet: ") + xslfile;
        return false;
    }
    style = xsltParseStylesheetDoc(styledoc);
    if (!style) {
        xmlFreeDoc(styledoc);
        errorMessage = _W("Impossible to load XSLT stylesheet: ") + xslfile;
        return false;
    }

    errorMessage.clear();
    lastXSLTError.clear();
    if (!overwrite && std::filesystem::is_regular_file(outputfile)) {
        errorMessage = _W("Destination file already exists: ") + outputfile;
        if (style) {
            xsltFreeStylesheet(style);
        }
        return false;
    }
    xsltSetGenericErrorFunc(nullptr, xsltErrorCapture);

    xmlDocPtr doc = xmlParseFile(wstring_to_utf8(xmlfile).c_str());
    if (!doc) {
        errorMessage = _W("Impossible to open or parse XML file: ") + xmlfile;
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

    xmlDocPtr result = xsltApplyStylesheet(style, doc, NULL);
    if (!result) {
        // Free stylesheet before freeing doc to avoid double-free of xmlDict
        if (style) {
            xsltFreeStylesheet(style);
            style = nullptr;
        }
        xmlFreeDoc(doc);
        if (!lastXSLTError.empty()) {
            errorMessage += utf8_to_wstring(lastXSLTError);
        } else {
            errorMessage = _W("Error applying XSLT stylesheet.");
        }
        return false;
    }

    std::string outf8 = wstring_to_utf8(outputfile);
    int saveResult = xsltSaveResultToFilename(outf8.c_str(), result, style, 0);

    // Free stylesheet before freeing result/doc to avoid xmlDict double-free on some platforms
    if (style) {
        xsltFreeStylesheet(style);
        style = nullptr;
    }

    xmlFreeDoc(result);
    xmlFreeDoc(doc);

    if (saveResult < 0) {
        errorMessage = _W("Error writing output file: ") + outputfile;
        return false;
    }

    return true;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
