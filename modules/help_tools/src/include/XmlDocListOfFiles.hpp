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
#include "XmlDocDocument.hpp"
#include "XmlTarget.hpp"
#include "nlsHelp_tools_exports.h"
#include <vector>
#include <string>
//=============================================================================
namespace Nelson {
//=============================================================================
class NLSHELP_TOOLS_IMPEXP XmlDocListOfFiles
{

private:
    std::wstring chapterTitle;
    std::wstring chapterDescription;
    std::wstring moduleName;
    std::wstring lastError;
    wstringVector srcFiles;
    std::wstring dstDirectory;
    bool bOverwriteExistingFiles;
    std::vector<XmlDocDocument*> xmlItems;
    void
    clearItems();
    std::wstring chapterResultFilename;
    DOCUMENT_OUTPUT outputTarget;
    std::wstring sectionUpName;
    std::wstring sectionUpUrl;

public:
    std::wstring
    getGeneratedChapterFilename();
    std::wstring
    getChapterTitle();
    std::wstring
    getModuleName();
    XmlDocListOfFiles(wstringVector srcFiles, const std::wstring& dstDirectory,
        bool bOverwriteExistingFiles = false, DOCUMENT_OUTPUT outputTarget = DOCUMENT_OUTPUT::HMTL);
    ~XmlDocListOfFiles();
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
    void
    setUpSection(const std::wstring& sectionName, const std::wstring& sectionUrl);
};
//=============================================================================
} // namespace Nelson
//=============================================================================
