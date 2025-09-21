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

static void
ext_replace(xmlXPathParserContextPtr ctxt, int nargs)
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

    // const char* input = reinterpret_cast<const char*>(str);

    //// Look for matching prefix in the mapping table
    // for (int i = 0; replace_table[i].placeholder != nullptr; ++i) {
    //     const char* ph = replace_table[i].placeholder;
    //     size_t ph_len = strlen(ph);

    //    if (strncmp(input, ph, ph_len) == 0) {
    //        const char* suffix = input + ph_len;
    //        xmlFree(str);

    //        xmlChar* out = xmlStrdup(reinterpret_cast<const xmlChar*>(replace_table[i].base_url));
    //        out = xmlStrcat(out, reinterpret_cast<const xmlChar*>(suffix));

    //        xmlXPathReturnString(ctxt, out);
    //        return;
    //    }
    //}

    // No match -> return unchanged
    xmlXPathReturnString(ctxt, str);
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
static xmlNodePtr
xmlNextNode(xmlNodePtr node)
{
    if (node->children)
        return node->children;
    while (node) {
        if (node->next)
            return node->next;
        node = node->parent;
    }
    return nullptr;
}
//=============================================================================
static std::vector<std::tuple<std::string, std::string, std::string>>
list_xml_keywords(const std::filesystem::path& srcDir)
{
    std::vector<std::tuple<std::string, std::string, std::string>> result;

    for (auto& entry : std::filesystem::recursive_directory_iterator(srcDir)) {
        if (!entry.is_regular_file())
            continue;
        if (entry.path().extension() != ".xml")
            continue;

        xmlDocPtr doc = xmlParseFile(entry.path().string().c_str());
        if (!doc)
            continue;

        xmlNodePtr root = xmlDocGetRootElement(doc);
        if (!root) {
            xmlFreeDoc(doc);
            continue;
        }

        std::string keyword, short_description;

        // Recherche <keyword>
        for (xmlNodePtr cur = root->children; cur; cur = cur->next) {
            if (cur->type == XML_ELEMENT_NODE && xmlStrEqual(cur->name, BAD_CAST "keyword")) {
                xmlChar* content = xmlNodeGetContent(cur);
                if (content) {
                    keyword = reinterpret_cast<const char*>(content);
                    xmlFree(content);
                }
                break;
            }
        }

        // Recherche <short_description>
        for (xmlNodePtr cur = root->children; cur; cur = cur->next) {
            if (cur->type == XML_ELEMENT_NODE
                && xmlStrEqual(cur->name, BAD_CAST "short_description")) {
                xmlChar* content = xmlNodeGetContent(cur);
                if (content) {
                    short_description = reinterpret_cast<const char*>(content);
                    xmlFree(content);
                }
                break;
            }
        }

        if (!keyword.empty()) {
            std::string relPath = std::filesystem::relative(entry.path(), srcDir).string();
            result.emplace_back(keyword, short_description, relPath);
        }

        xmlFreeDoc(doc);
    }

    return result;
}
//=============================================================================
bool
XmlTransform(const std::wstring& xmlfile, const std::wstring& xslfile,
    const std::wstring& outputfile, bool overwrite, DOCUMENT_OUTPUT documentOutput,
    std::wstring& errorMessage)
{

    xsltStylesheetPtr style = nullptr;
    xmlDocPtr styledoc = xmlParseFile(wstring_to_utf8(xslfile).c_str());
    if (!styledoc) {
        errorMessage = L"Impossible de charger la feuille de style XSLT : " + xslfile;
        return false;
    }
    style = xsltParseStylesheetDoc(styledoc);
    if (!style) {
        errorMessage = L"Impossible de charger la feuille de style XSLT : " + xslfile;
        return false;
    }
    bool res
        = XmlTransform(xmlfile, (void*)style, outputfile, overwrite, documentOutput, errorMessage);
    xsltFreeStylesheet(style);

    if (!res) {
        return false;
    }
    return true;
}
//=============================================================================
bool
XmlTransform(const std::wstring& xmlfile, void* _style, const std::wstring& outputfile,
    bool overwrite, DOCUMENT_OUTPUT documentOutput, std::wstring& errorMessage)
{
    errorMessage.clear();
    lastXSLTError.clear();
    if (!overwrite && std::filesystem::is_regular_file(outputfile)) {
        errorMessage = L"Destination file already exist : " + outputfile;
        return false;
    }
    xsltSetGenericErrorFunc(nullptr, xsltErrorCapture);

    if (documentOutput != DOCUMENT_OUTPUT::MARKDOWN) {
        xsltRegisterExtModuleFunction((const xmlChar*)"replace",
            (const xmlChar*)"http://io.github.nelson_lang/ext", Nelson::ext_remove_module_prefix);
    }
    xsltRegisterExtModuleFunction((const xmlChar*)"copy_img",
        (const xmlChar*)"http://io.github.nelson_lang/ext", Nelson::ext_copy_img);

    xmlDocPtr doc = xmlParseFile(wstring_to_utf8(xmlfile).c_str());
    if (!doc) {
        errorMessage = L"Impossible d'ouvrir ou de parser le fichier XML : " + xmlfile;
        return false;
    }

    std::string outputDir
        = std::filesystem::path(wstring_to_utf8(outputfile)).parent_path().string();
    if (outputDir.empty()) {
        outputDir = ".";
    }

    xsltStylesheetPtr style = (xsltStylesheetPtr)_style;

    xmlSetProp(xmlDocGetRootElement(doc), BAD_CAST "outputdir", BAD_CAST outputDir.c_str());
    xmlSetProp(xmlDocGetRootElement(doc), BAD_CAST "xmlfilename",
        BAD_CAST wstring_to_utf8(xmlfile).c_str());

    xmlDocPtr result = xsltApplyStylesheet(style, doc, NULL);
    if (!result) {
        xmlFreeDoc(doc);
        if (!lastXSLTError.empty()) {
            errorMessage += utf8_to_wstring(lastXSLTError);
        } else {
            errorMessage = L"Erreur lors de l'application de la feuille de style XSLT.";
        }
        return false;
    }

    std::string outf8 = wstring_to_utf8(outputfile);
    int saveResult = xsltSaveResultToFilename(outf8.c_str(), result, style, 0);
    if (saveResult < 0) {
        xmlFreeDoc(result);
        xmlFreeDoc(doc);
        errorMessage = L"Erreur lors de l'écriture du fichier de sortie : " + outputfile;
        return false;
    }

    xmlFreeDoc(result);
    xmlFreeDoc(doc);

    return true;
}
//=============================================================================
}
//=============================================================================
