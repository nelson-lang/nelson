//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "XmlDocBuild.hpp"
#include <filesystem>
#include "i18n.hpp"
#include "XmlTransform.hpp"
#include "XmlDocListOfFiles.hpp"
#include "ModulesManager.hpp"
#include "StringHelpers.hpp"
#include "characters_encoding.hpp"
#include "XmlDocToc.hpp"
#include "XmlDocSummary.hpp"
#include <libxslt/xslt.h>
#include <libxslt/transform.h>
#include <libxslt/xsltutils.h>
#include <libxslt/extensions.h>
#include <mutex>
#include "NelsonConfiguration.hpp"
#include "omp_for_loop.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
xmldocbuild(const wstringVector& srcDir, std::wstring destDir, const std::wstring& mainTitle,
    DOCUMENT_OUTPUT documentOutput, bool overwrite, std::wstring& errorMessage)
{
    errorMessage.clear();
    std::vector<XMLDOCFILES> xmldocFilesList;
    std::string language;

    if (!xmlDocListOfFiles(srcDir, xmldocFilesList, language, errorMessage)) {
        return false;
    }

    std::wstring helptoolsPath;
    ModulesManager::Instance().findModule(L"help_tools", helptoolsPath);

    if (documentOutput == DOCUMENT_OUTPUT::HTML_WEB) {
        std::wstring xsltTocHtmlFilename = helptoolsPath + L"/resources/nelson_toc2html.xslt";
        std::wstring xsltSummaryHtmlFilename
            = helptoolsPath + L"/resources/nelson_summary2html.xslt";

        if (!XmlDocTocSummary(destDir, xmldocFilesList, xsltTocHtmlFilename,
                xsltSummaryHtmlFilename, documentOutput, errorMessage)) {
            return false;
        }
    }

    if (documentOutput == DOCUMENT_OUTPUT::MARKDOWN) {
        std::wstring xsltTocHtmlFilename = helptoolsPath + L"/resources/nelson_toc2md.xslt";
        std::wstring xsltSummaryHtmlFilename = helptoolsPath + L"/resources/nelson_summary2md.xslt";

        if (!XmlDocTocSummary(destDir, xmldocFilesList, xsltTocHtmlFilename,
                xsltSummaryHtmlFilename, documentOutput, errorMessage)) {
            return false;
        }
    }

    xsltStylesheetPtr style = nullptr;

    std::wstring extDestination = L".html";

    if (documentOutput == DOCUMENT_OUTPUT::HTML_WEB) {
        extDestination = L".html";
        std::wstring xsltHtmlFilename = helptoolsPath + L"/resources/nelson_html.xslt";
        xmlDocPtr styledoc = xmlParseFile(wstring_to_utf8(xsltHtmlFilename).c_str());
        if (!styledoc) {
            errorMessage = L"Unable to load XSLT stylesheet: " + xsltHtmlFilename;
            return false;
        }
        style = xsltParseStylesheetDoc(styledoc);
        if (!style) {
            xmlFreeDoc(styledoc);
            errorMessage = L"Unable to load XSLT stylesheet: " + xsltHtmlFilename;
            return false;
        }
    }
    if (documentOutput == DOCUMENT_OUTPUT::MARKDOWN) {
        extDestination = L".md";
        std::wstring xsltHtmlFilename = helptoolsPath + L"/resources/nelson_markdown.xslt";
        xmlDocPtr styledoc = xmlParseFile(wstring_to_utf8(xsltHtmlFilename).c_str());
        if (!styledoc) {
            errorMessage = L"Unable to load XSLT stylesheet: " + xsltHtmlFilename;
            return false;
        }
        style = xsltParseStylesheetDoc(styledoc);
        if (!style) {
            xmlFreeDoc(styledoc);
            errorMessage = L"Unable to load XSLT stylesheet: " + xsltHtmlFilename;
            return false;
        }
    }

    std::mutex errorMutex;
    wstringVector errors;
    const size_t numFiles = xmldocFilesList.size();

    for (int j = 0; j < numFiles; ++j) {
        auto& xmlDocInfo = xmldocFilesList[j];
        std::wstring moduleName = std::get<1>(xmlDocInfo);
        std::vector<XMLDOCFILE>& xmlDocFiles = std::get<4>(xmlDocInfo);

        OMP_PARALLEL_FOR_LOOP(xmlDocFiles.size(), 8)
        for (int k = 0; k < xmlDocFiles.size(); ++k) {
            std::wstring localError;
            std::wstring xmlFilename = std::get<2>(xmlDocFiles[k]);
            std::wstring completeXmlFilename = std::get<0>(xmlDocInfo) + L"/" + xmlFilename;
            std::wstring destinationFilename = xmlFilename;

            StringHelpers::replace_last(destinationFilename, L".xml", extDestination);
            destinationFilename = destDir + L"/" + destinationFilename;

            if (!XmlTransform(completeXmlFilename, style, destinationFilename, overwrite,
                    documentOutput, localError)) {
                std::lock_guard<std::mutex> lock(errorMutex);
                errors.push_back(std::move(localError));
            }
        }
    }

    xsltFreeStylesheet(style);

    if (!errors.empty()) {
        errorMessage = std::move(errors[0]);
        return false;
    }

    return true;
}
//=============================================================================
}
//=============================================================================
