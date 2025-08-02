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
#include "Types.hpp"
#include "XmlDocDirectory.hpp"
#include "XmlDocDocument.hpp"
#include "XmlDocMainIndex.hpp"
#include "XmlTarget.hpp"
#include "nlsHelp_tools_exports.h"
#include <vector>
#include <string>
//=============================================================================
namespace Nelson {
//=============================================================================
class NLSHELP_TOOLS_IMPEXP XmlDocListOfDirectories
{

private:
    wstringVector srcDirectories;
    std::wstring dstDirectory;
    std::wstring mainTitle;
    std::vector<XmlDocDirectory*> itemsDirectories;
    bool bOverwriteExistingFiles;
    std::wstring lastError;
    void
    clearItems();
    DOCUMENT_OUTPUT outputTarget;
    XmlDocMainIndex* mainIndex;

public:
    XmlDocListOfDirectories(wstringVector srcDirectories, const std::wstring& dstDirectory,
        const std::wstring& mainTitle, bool bOverwriteExistingFiles = false,
        DOCUMENT_OUTPUT outputTarget = DOCUMENT_OUTPUT::HMTL);
    ~XmlDocListOfDirectories();
    bool
    read();
    bool
    writeAsHtml();
    bool
    writeAsMarkdown();
    std::wstring
    getLastError();
    void
    getIndex(wstringVector& names, wstringVector& urls, wstringVector& descriptions);
    std::wstring
    getOutputHelpBasename();
};
//=============================================================================
} // namespace Nelson
//=============================================================================
