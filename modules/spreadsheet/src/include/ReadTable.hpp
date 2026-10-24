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
#include "ArrayOf.hpp"
#include "nlsSpreadsheet_exports.h"
#include "DetectImportOptions.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
NLSSPREADSHEET_IMPEXP ArrayOf
ReadTable(
    const std::wstring& filename, const detectImportOptions& options, std::string& errorMessage);
//=============================================================================
// Reads a delimited text file restricted to a rectangular Range (e.g. "A1:B5",
// "A2", "A:B", "2:4", or a canonical corner:corner string). 'readVariableNames'
// is tri-state: -1 auto-detect from the first row of the range, 0 forced off,
// 1 forced on. On an unparsable range, sets errorMessage and returns an empty value.
NLSSPREADSHEET_IMPEXP ArrayOf
ReadTableRange(const std::wstring& filename, const detectImportOptions& options,
    const std::wstring& rangeSpec, int readVariableNames, std::string& errorMessage);
//=============================================================================
} // namespace Nelson
//=============================================================================
