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
#include "XmlDocDocument.hpp"
#include "XmlDocListOfFiles.hpp"
#include "XmlTarget.hpp"
#include "nlsHelp_tools_exports.h"
#include <vector>
#include <string>
//=============================================================================
namespace Nelson {
//=============================================================================
class NLSHELP_TOOLS_IMPEXP XmlDocDirectory
{

private:
    std::wstring srcDirectory;
    std::wstring dstDirectory;
    XmlDocListOfFiles* xmlDocFiles;
    DOCUMENT_OUTPUT outputTarget;
    std::wstring sectionUpName;
    std::wstring sectionUpUrl;

public:
    XmlDocDirectory(const std::wstring& srcDirectory, const std::wstring& dstDirectory,
        bool bOverwriteExistingFiles = false, DOCUMENT_OUTPUT outputTarget = DOCUMENT_OUTPUT::HMTL);
    ~XmlDocDirectory();
    bool
    read();
    bool
    writeAsHtml();
    bool
    writeAsMarkdown();
    std::wstring
    getLastError();
    std::wstring
    getGeneratedChapterFilename();
    std::wstring
    getChapterTitle();
    std::wstring
    getModuleName();
    void
    setUpSection(const std::wstring& sectionName, const std::wstring& sectionUrl);
    void
    getIndex(wstringVector& names, wstringVector& urls, wstringVector& descriptions);
};
//=============================================================================
} // namespace Nelson
//=============================================================================
