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
#include <string>
#include <vector>
//=============================================================================
namespace Nelson {
//=============================================================================
struct XmlDocPage
{
    std::vector<std::string> keywordAndAlias;
    std::string shortDescription;
    std::wstring absoluteFilename;
    std::wstring relativeFilename;
};
//=============================================================================
struct XmlDocSection
{
    std::wstring directory;
    std::wstring relativeDirectory;
    std::wstring moduleName;
    std::wstring chapterTitle;
    std::string chapterDescription;
    std::vector<XmlDocPage> pages;
    std::vector<XmlDocSection> children;
};
//=============================================================================
bool
xmlDocListOfFiles(const wstringVector& xmlDirectories, std::vector<XmlDocSection>& xmlDocFiles,
    std::string& language, std::wstring& errorMessage);
//=============================================================================
}
//=============================================================================
