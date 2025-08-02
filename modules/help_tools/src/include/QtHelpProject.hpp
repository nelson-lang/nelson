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
#include "nlsHelp_tools_exports.h"
#include <string>
//=============================================================================
namespace Nelson {
//=============================================================================
class NLSHELP_TOOLS_IMPEXP QtHelpProject
{
private:
    std::wstring destdirectory;
    std::wstring filenameDestination;
    std::string utf8stream;
    std::wstring mainTitle;
    std::wstring name_space;
    std::wstring virtualfolder;

    wstringVector sectionsName;
    wstringVector sectionsUrl;

    wstringVector keywordsName;
    wstringVector keywordsUrl;

    void
    assembleContent();

    std::string
    uuid_module();

public:
    QtHelpProject(const std::wstring& destdirectory, std::wstring mainTitle,
        std::wstring name_space, std::wstring virtualfolder = L"help");
    ~QtHelpProject();
    std::wstring
    getFilename();
    bool
    write();
    void
    appendSection(const std::wstring& sectionName, const std::wstring& sectionUrl,
        const wstringVector& names, const wstringVector& urls);
};
//=============================================================================
} // namespace Nelson
//=============================================================================
