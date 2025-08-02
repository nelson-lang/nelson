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
#include "QtHelpProject.hpp"
#include "Types.hpp"
#include "XmlTarget.hpp"
#include "nlsHelp_tools_exports.h"
#include <string>
//=============================================================================
namespace Nelson {
//=============================================================================
class NLSHELP_TOOLS_IMPEXP XmlDocMainIndex
{
private:
    std::wstring directoryDestination;
    std::wstring filenameDestination;
    std::string utf8stream;
    std::wstring mainTitle;
    std::wstring mainModuleShortName;
    bool isQtHelp;
    DOCUMENT_OUTPUT outputTarget;
    void
    htmlHeader();
    void
    htmlOpenTags();
    void
    htmlCloseTags();
    QtHelpProject* qtproject;

public:
    XmlDocMainIndex(const std::wstring& destdir, const std::wstring& mainTitle,
        const std::wstring& mainModuleShortName,
        DOCUMENT_OUTPUT outputTarget = DOCUMENT_OUTPUT::HMTL);
    ~XmlDocMainIndex();
    std::wstring
    getFilename();
    bool
    writeAsHtml();
    bool
    writeAsMarkdown();
    void
    appendSection(const std::wstring& sectionName, const std::wstring& sectionUrl,
        const wstringVector& names, const wstringVector& urls, const wstringVector& descriptions);
};
//=============================================================================
} // namespace Nelson
//=============================================================================
