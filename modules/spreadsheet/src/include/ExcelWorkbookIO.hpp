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
#include <string>
//=============================================================================
namespace Nelson {
//=============================================================================
class NLSSPREADSHEET_IMPEXP excelWorkbookOptions
{
public:
    std::wstring sheetName;
    indexType sheetIndex = 1;
    bool hasSheet = false;
    bool sheetIsIndex = false;
    std::wstring range;
    bool hasRange = false;
    bool useExcel = false;
    bool writeVariableNames = true;
    bool writeRowNames = false;
    bool readVariableNames = true;
    bool readVariableNamesSet = false;
    std::string writeMode = "inplace";
    std::string textType = "char";
};
//=============================================================================
NLSSPREADSHEET_IMPEXP bool
isExcelWorkbookExtension(const std::wstring& filename);
//=============================================================================
NLSSPREADSHEET_IMPEXP ArrayOf
ReadExcelCell(
    const std::wstring& filename, const excelWorkbookOptions& options, std::wstring& errorMessage);
//=============================================================================
NLSSPREADSHEET_IMPEXP ArrayOf
ReadExcelMatrix(const std::wstring& filename, const excelWorkbookOptions& options,
    NelsonType outputType, std::wstring& errorMessage);
//=============================================================================
NLSSPREADSHEET_IMPEXP ArrayOf
ReadExcelTable(
    const std::wstring& filename, const excelWorkbookOptions& options, std::wstring& errorMessage);
//=============================================================================
NLSSPREADSHEET_IMPEXP bool
WriteExcelArray(const ArrayOf& value, const std::wstring& filename,
    const excelWorkbookOptions& options, std::wstring& errorMessage);
//=============================================================================
NLSSPREADSHEET_IMPEXP bool
WriteExcelTable(const ArrayOf& table, const std::wstring& filename,
    const excelWorkbookOptions& options, std::wstring& errorMessage);
//=============================================================================
NLSSPREADSHEET_IMPEXP bool
ExcelWorkbookInfo(const std::wstring& filename, stringVector& sheetNames, std::wstring& format,
    std::wstring& errorMessage);
//=============================================================================
} // namespace Nelson
//=============================================================================
