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
#include <string>
#include <vector>
#include <tuple>
#include "Types.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
// keywordAndAlias, short description, xml file
using XMLDOCFILE = std::tuple<std::vector<std::string>, std::string, std::wstring>;
// directory, module name, chapter title, vector of XMLDOCFILE
using XMLDOCFILES = std::tuple<std::wstring, // directory
    std::wstring, // module name
    std::wstring, // chapter title
    std::string, // chapter description
    std::vector<XMLDOCFILE>>;
//=============================================================================
bool
xmlDocListOfFiles(const wstringVector& xmlDirectories, std::vector<XMLDOCFILES>& xmlDocFiles,
    std::string& language, std::wstring& errorMessage);
//=============================================================================
}
//=============================================================================
