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
#include "DetectImportOptions.hpp"
//=============================================================================
namespace Nelson::SpreadsheetGateway {
//=============================================================================
constexpr const char* NELSON_DELIMITED_TEXT_IMPORT_OPTIONS_CLASS
    = "nelson.io.text.DelimitedTextImportOptions";
//=============================================================================
constexpr const char* NELSON_SPREADSHEET_IMPORT_OPTIONS_CLASS
    = "nelson.io.spreadsheet.SpreadsheetImportOptions";
//=============================================================================
bool
isDelimitedTextImportOptions(const ArrayOf& value);
//=============================================================================
bool
isSpreadsheetImportOptions(const ArrayOf& value);
//=============================================================================
bool
isImportOptions(const ArrayOf& value);
//=============================================================================
ArrayOf
toDelimitedTextImportOptions(const detectImportOptions& options);
//=============================================================================
void
populateImportOptions(const ArrayOf& importOptionsObj, detectImportOptions& options);
//=============================================================================
// Fill the per-variable fields (names, declared types, per-variable input
// formats and whitespace rules) shared by both import-options classes.
void
populateVariableImportOptions(const ArrayOf& importOptionsObj, detectImportOptions& options);
//=============================================================================
} // namespace Nelson::SpreadsheetGateway
//=============================================================================
