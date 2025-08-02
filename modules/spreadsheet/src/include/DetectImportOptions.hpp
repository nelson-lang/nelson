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
#include "nlsSpreadsheet_exports.h"
#include "Types.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
#ifdef _MSC_VER
#pragma warning(push)
#pragma warning(disable : 4251)
#endif
class NLSSPREADSHEET_IMPEXP detectImportOptions
{
public:
    std::vector<std::string> Delimiter;
    std::vector<std::string> LineEnding;
    std::vector<std::string> CommentStyle;
    std::string EmptyLineRule;
    std::string TextType;
    int VariableNamesLine;
    int RowNamesColumn;
    std::vector<std::string> VariableNames;
    std::vector<double> DataLines;
};
#ifdef _MSC_VER
#pragma warning(pop)
#endif
//=============================================================================
NLSSPREADSHEET_IMPEXP void
initializeDetectImportOptions(detectImportOptions& options);
//=============================================================================
NLSSPREADSHEET_IMPEXP void
analyzeFileFormatImportOptions(std::wstring filename, size_t sampleSize,
    detectImportOptions& options, std::string& errorMessage);
//=============================================================================
}
//=============================================================================
