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
#include "XmlDocLinkIndex.hpp"
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
    wstringVector warnings;
    return xmldocbuild(srcDir, destDir, mainTitle, documentOutput, overwrite, errorMessage,
        wstringVector(), warnings);
}
//=============================================================================
bool
xmldocbuild(const wstringVector& srcDir, std::wstring destDir, const std::wstring& mainTitle,
    DOCUMENT_OUTPUT documentOutput, bool overwrite, std::wstring& errorMessage,
    const wstringVector& indexDirectories, wstringVector& warnings)
{
    errorMessage.clear();
    warnings.clear();
    std::vector<XmlDocSection> xmldocFilesList;
    std::string language;

    if (!xmlDocListOfFiles(srcDir, xmldocFilesList, language, errorMessage)) {
        return false;
    }
    (void)language;

    // Link index over every module that pages may reference, current one included.
    wstringVector indexRoots = indexDirectories;
    indexRoots.insert(indexRoots.end(), srcDir.begin(), srcDir.end());
    std::shared_ptr<const XmlDocLinkIndex> linkIndex
        = xmlDocLinkIndexForDirectories(indexRoots, errorMessage);
    if (!linkIndex) {
        return false;
    }
    wstringVector currentModules;
    for (const auto& xmlDocInfo : xmldocFilesList) {
        if (!xmlDocInfo.moduleName.empty()) {
            currentModules.push_back(xmlDocInfo.moduleName);
        }
    }
    XmlDocLinkResolver linkResolver(linkIndex, currentModules, documentOutput);

    std::wstring extDestination = documentOutputExtension(documentOutput);
    std::vector<PageBuildItem> pageBuildItems;
    for (const auto& xmlDocInfo : xmldocFilesList) {
        collectPages(xmlDocInfo, extDestination, pageBuildItems);
    }

    std::wstring helptoolsPath;
    ModulesManager::Instance().findModule(L"help_tools", helptoolsPath);
    std::wstring resourcesPath = helptoolsPath + L"/resources/";

    std::wstring xsltFilename;
    std::wstring xsltTocFilename;
    std::wstring xsltSummaryFilename;
    switch (documentOutput) {
    case DOCUMENT_OUTPUT::MARKDOWN:
        xsltFilename = resourcesPath + L"nelson_markdown.xslt";
        xsltTocFilename = resourcesPath + L"nelson_toc2md.xslt";
        xsltSummaryFilename = resourcesPath + L"nelson_summary2md.xslt";
        break;
    case DOCUMENT_OUTPUT::TYPST:
        xsltFilename = resourcesPath + L"nelson_typst.xslt";
        xsltTocFilename = resourcesPath + L"nelson_toc2typ.xslt";
        xsltSummaryFilename = resourcesPath + L"nelson_summary2typ.xslt";
        break;
    case DOCUMENT_OUTPUT::HTML_WEB:
    default:
        xsltFilename = resourcesPath + L"nelson_html.xslt";
        xsltTocFilename = resourcesPath + L"nelson_toc2html.xslt";
        xsltSummaryFilename = resourcesPath + L"nelson_summary2html.xslt";
        break;
    }
    if (!XmlDocTocSummary(destDir, xmldocFilesList, xsltTocFilename, xsltSummaryFilename,
            documentOutput, errorMessage, &linkResolver)) {
        return false;
    }

    // Typst pages are labelled "<module:page>" so cross-references resolve
    // inside an assembled document; the module comes from the root chapter.
    std::wstring moduleName = mainTitle;
    if (!xmldocFilesList.empty() && !xmldocFilesList.front().moduleName.empty()) {
        moduleName = xmldocFilesList.front().moduleName;
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
                overwrite, documentOutput, localError, destDir, item.canonicalRelativeFilename,
                moduleName, &linkResolver)) {
            std::lock_guard<std::mutex> lock(errorMutex);
            errors.push_back(std::move(localError));
        }
    }
    warnings = linkResolver.warnings();
    if (!errors.empty()) {
        errorMessage = std::move(errors[0]);
        return false;
    }

    return true;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
