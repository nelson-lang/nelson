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
#include "ModulesManager.hpp"
#include "NelsonConfiguration.hpp"
#include "StringHelpers.hpp"
#include "XmlDocListOfFiles.hpp"
#include "XmlDocSummary.hpp"
#include "XmlDocToc.hpp"
#include "XmlTransform.hpp"
#include "i18n.hpp"
#include "omp_for_loop.hpp"
#include <filesystem>
#include <mutex>
//=============================================================================
namespace Nelson {
//=============================================================================
struct PageBuildItem
{
    const XmlDocPage* page;
    std::wstring canonicalRelativeFilename;
};
//=============================================================================
static std::wstring
getDestinationFileExtension(DOCUMENT_OUTPUT documentOutput)
{
    switch (documentOutput) {
    case DOCUMENT_OUTPUT::HTML_WEB:
        return L".html";
    case DOCUMENT_OUTPUT::MARKDOWN:
        return L".md";
    default:
        return L".html";
    }
}
//=============================================================================
static std::wstring
replaceXmlExtension(std::wstring filename, const std::wstring& extension)
{
    StringHelpers::replace_last(filename, L".xml", extension);
    return filename;
}
//=============================================================================
static void
collectPages(const XmlDocSection& section, const std::wstring& extension,
    std::vector<PageBuildItem>& pageBuildItems)
{
    for (const auto& page : section.pages) {
        PageBuildItem item;
        item.page = &page;
        item.canonicalRelativeFilename = replaceXmlExtension(page.relativeFilename, extension);
        pageBuildItems.push_back(item);
    }
    for (const auto& child : section.children) {
        collectPages(child, extension, pageBuildItems);
    }
}
//=============================================================================
bool
xmldocbuild(const wstringVector& srcDir, std::wstring destDir, const std::wstring& mainTitle,
    DOCUMENT_OUTPUT documentOutput, bool overwrite, std::wstring& errorMessage)
{
    (void)mainTitle;
    errorMessage.clear();
    std::vector<XmlDocSection> xmldocFilesList;
    std::string language;

    if (!xmlDocListOfFiles(srcDir, xmldocFilesList, language, errorMessage)) {
        return false;
    }
    (void)language;

    std::wstring extDestination = getDestinationFileExtension(documentOutput);
    std::vector<PageBuildItem> pageBuildItems;
    for (const auto& xmlDocInfo : xmldocFilesList) {
        collectPages(xmlDocInfo, extDestination, pageBuildItems);
    }

    std::wstring helptoolsPath;
    ModulesManager::Instance().findModule(L"help_tools", helptoolsPath);

    std::wstring xsltFilename;
    if (documentOutput == DOCUMENT_OUTPUT::HTML_WEB) {
        std::wstring xsltTocHtmlFilename = helptoolsPath + L"/resources/nelson_toc2html.xslt";
        std::wstring xsltSummaryHtmlFilename
            = helptoolsPath + L"/resources/nelson_summary2html.xslt";

        if (!XmlDocTocSummary(destDir, xmldocFilesList, xsltTocHtmlFilename,
                xsltSummaryHtmlFilename, documentOutput, errorMessage)) {
            return false;
        }
        xsltFilename = helptoolsPath + L"/resources/nelson_html.xslt";
    }

    if (documentOutput == DOCUMENT_OUTPUT::MARKDOWN) {
        std::wstring xsltTocHtmlFilename = helptoolsPath + L"/resources/nelson_toc2md.xslt";
        std::wstring xsltSummaryHtmlFilename = helptoolsPath + L"/resources/nelson_summary2md.xslt";

        if (!XmlDocTocSummary(destDir, xmldocFilesList, xsltTocHtmlFilename,
                xsltSummaryHtmlFilename, documentOutput, errorMessage)) {
            return false;
        }
        xsltFilename = helptoolsPath + L"/resources/nelson_markdown.xslt";
    }

    std::mutex errorMutex;
    wstringVector errors;

    OMP_PARALLEL_FOR_LOOP(pageBuildItems.size(), 8)
    for (int k = 0; k < pageBuildItems.size(); ++k) {
        const PageBuildItem& item = pageBuildItems[k];
        std::wstring localError;
        std::filesystem::path destinationFilename
            = std::filesystem::path(destDir) / item.canonicalRelativeFilename;
        std::filesystem::create_directories(destinationFilename.parent_path());

        if (!XmlTransform(item.page->absoluteFilename, xsltFilename, destinationFilename.wstring(),
                overwrite, documentOutput, localError, destDir, item.canonicalRelativeFilename)) {
            std::lock_guard<std::mutex> lock(errorMutex);
            errors.push_back(std::move(localError));
        }
    }
    if (!errors.empty()) {
        errorMessage = std::move(errors[0]);
        return false;
    }

    return true;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
