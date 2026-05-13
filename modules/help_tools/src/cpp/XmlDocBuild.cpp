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
#include "characters_encoding.hpp"
#include "i18n.hpp"
#include "omp_for_loop.hpp"
#include <filesystem>
#include <fstream>
#include <map>
#include <mutex>
//=============================================================================
namespace Nelson {
//=============================================================================
struct PageBuildItem
{
    const XmlDocPage* page;
    std::wstring canonicalRelativeFilename;
    std::wstring aliasRelativeFilename;
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
        item.aliasRelativeFilename = replaceXmlExtension(
            std::filesystem::path(page.relativeFilename).filename().wstring(), extension);
        pageBuildItems.push_back(item);
    }
    for (const auto& child : section.children) {
        collectPages(child, extension, pageBuildItems);
    }
}
//=============================================================================
static std::string
htmlEscape(const std::string& value)
{
    std::string escaped;
    escaped.reserve(value.size());
    for (char c : value) {
        switch (c) {
        case '&':
            escaped += "&amp;";
            break;
        case '<':
            escaped += "&lt;";
            break;
        case '>':
            escaped += "&gt;";
            break;
        case '"':
            escaped += "&quot;";
            break;
        case '\'':
            escaped += "&#39;";
            break;
        default:
            escaped.push_back(c);
            break;
        }
    }
    return escaped;
}
//=============================================================================
static bool
writeHtmlRedirectAlias(const std::filesystem::path& aliasFile,
    const std::wstring& canonicalRelativeFilename, bool overwrite, std::wstring& errorMessage)
{
    if (!overwrite && std::filesystem::is_regular_file(aliasFile)) {
        errorMessage = _W("Destination file already exists: ") + aliasFile.wstring();
        return false;
    }
    std::string canonicalUtf8 = wstring_to_utf8(canonicalRelativeFilename);
    std::string escapedCanonical = htmlEscape(canonicalUtf8);

    std::ofstream out;
#ifdef _MSC_VER
    out.open(aliasFile, std::ios::out | std::ios::binary);
#else
    out.open(aliasFile.string(), std::ios::out | std::ios::binary);
#endif
    if (!out.is_open()) {
        errorMessage = _W("Cannot create alias file: ") + aliasFile.wstring();
        return false;
    }
    out << "<!DOCTYPE html>\n"
        << "<html><head><meta charset=\"UTF-8\">\n"
        << "<meta http-equiv=\"refresh\" content=\"0; url=" << escapedCanonical << "\">\n"
        << "<script>window.location.replace(\"" << escapedCanonical << "\");</script>\n"
        << "</head><body><a href=\"" << escapedCanonical << "\">" << escapedCanonical
        << "</a></body></html>\n";
    out.close();
    return true;
}
//=============================================================================
static bool
copyMarkdownAlias(const std::filesystem::path& canonicalFile,
    const std::filesystem::path& aliasFile, bool overwrite, std::wstring& errorMessage)
{
    if (!overwrite && std::filesystem::is_regular_file(aliasFile)) {
        errorMessage = _W("Destination file already exists: ") + aliasFile.wstring();
        return false;
    }
    std::error_code ec;
    std::filesystem::copy_file(canonicalFile, aliasFile,
        overwrite ? std::filesystem::copy_options::overwrite_existing
                  : std::filesystem::copy_options::none,
        ec);
    if (ec) {
        errorMessage = _W("Cannot create alias file: ") + aliasFile.wstring();
        return false;
    }
    return true;
}
//=============================================================================
static bool
checkAliasCollisions(const std::vector<PageBuildItem>& pageBuildItems, std::wstring& errorMessage)
{
    std::map<std::wstring, std::wstring> aliases;
    for (const auto& item : pageBuildItems) {
        auto [it, inserted]
            = aliases.emplace(item.aliasRelativeFilename, item.canonicalRelativeFilename);
        if (!inserted && it->second != item.canonicalRelativeFilename) {
            errorMessage = _W("Help page alias collision: ") + item.aliasRelativeFilename + L" ("
                + it->second + L" vs " + item.canonicalRelativeFilename + L")";
            return false;
        }
    }
    return true;
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
    if (!checkAliasCollisions(pageBuildItems, errorMessage)) {
        return false;
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

    for (const auto& item : pageBuildItems) {
        if (item.aliasRelativeFilename == item.canonicalRelativeFilename) {
            continue;
        }
        std::filesystem::path canonicalFile
            = std::filesystem::path(destDir) / item.canonicalRelativeFilename;
        std::filesystem::path aliasFile
            = std::filesystem::path(destDir) / item.aliasRelativeFilename;
        if (documentOutput == DOCUMENT_OUTPUT::HTML_WEB) {
            if (!writeHtmlRedirectAlias(
                    aliasFile, item.canonicalRelativeFilename, overwrite, errorMessage)) {
                return false;
            }
        } else if (documentOutput == DOCUMENT_OUTPUT::MARKDOWN) {
            if (!copyMarkdownAlias(canonicalFile, aliasFile, overwrite, errorMessage)) {
                return false;
            }
        }
    }

    return true;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
