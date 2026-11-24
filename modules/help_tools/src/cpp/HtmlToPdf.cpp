//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "HtmlToPdf.hpp"
#include "HtmlToPdfProvider.hpp"
#include "characters_encoding.hpp"
#include <chrono>
#include <filesystem>
#include <fstream>
//=============================================================================
namespace Nelson {
//=============================================================================
// HTML to PDF conversion is served by the web stack (nelson-webview) through the
// nelson_manager seam. help_tools stays free of any desktop web dependency; when
// no provider is registered the conversion is reported as unavailable.
//=============================================================================
bool
HtmlFileToPdfFile(const std::wstring& htmlsrcfilename, const std::wstring& pdfdestfilename,
    std::wstring& errorMessage)
{
    errorMessage.clear();
    return runHtmlToPdf(htmlsrcfilename, pdfdestfilename, errorMessage);
}
//=============================================================================
bool
HtmlStreamToPdfFile(
    const std::wstring& htmlstream, const std::wstring& pdfdestfilename, std::wstring& errorMessage)
{
    errorMessage.clear();
    std::error_code ec;
    std::filesystem::path base = std::filesystem::temp_directory_path(ec);
    if (ec || base.empty()) {
        base = std::filesystem::current_path();
    }
    const auto stamp = std::chrono::high_resolution_clock::now().time_since_epoch().count();
    std::filesystem::path htmlPath
        = base / ("nelson_htmltopdf_stream_" + std::to_string(stamp) + ".html");

    std::ofstream htmlFile(htmlPath, std::ios::binary);
    if (!htmlFile.is_open()) {
        return false;
    }
    const std::string utf8Html = wstring_to_utf8(htmlstream);
    htmlFile.write(utf8Html.data(), static_cast<std::streamsize>(utf8Html.size()));
    htmlFile.close();

    const bool ok = runHtmlToPdf(htmlPath.wstring(), pdfdestfilename, errorMessage);
    std::filesystem::remove(htmlPath, ec);
    return ok;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
