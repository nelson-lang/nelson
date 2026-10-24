//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include "nlsXml_exports.h"
#include "XmlTarget.hpp"
#include <string>
//=============================================================================
namespace Nelson {
//=============================================================================
// Destination of a resolved <link linkend="...">.
struct XmlResolvedLink
{
    // Relative hyperlink from the output file, extension included.
    std::string href;
    std::string targetModule;
    // Target page path relative to its module root, without extension
    // ("plots/advanced/mesh").
    std::string targetPage;
};
//=============================================================================
// Resolves <link linkend="..."> targets during an XML transform. Each resolved
// <link> gets the attributes "href", "target_module" and "target_page", which the
// stylesheets use instead of rebuilding a path from the linkend.
class XmlLinkResolver
{
public:
    virtual ~XmlLinkResolver() = default;
    // rootPathPrefix is the relative path from the output file directory to the
    // root output directory ("" when the file is at the root, "../", ...).
    virtual bool
    resolve(const std::string& linkend, const std::string& rootPathPrefix,
        const std::wstring& xmlfile, XmlResolvedLink& resolved)
        = 0;
    virtual void
    unresolved(const std::string& linkend, const std::wstring& xmlfile)
        = 0;
};
//=============================================================================
NLSXML_IMPEXP bool
XmlTransform(const std::wstring& xmlfile, const std::wstring& xslfile,
    const std::wstring& outputfile, bool overwrite, DOCUMENT_OUTPUT documentOutput,
    std::wstring& errorMessage, const std::wstring& rootOutputDirectory = L"",
    const std::wstring& canonicalLink = L"", const std::wstring& moduleName = L"",
    XmlLinkResolver* linkResolver = nullptr);
//=============================================================================
} // namespace Nelson
//=============================================================================
