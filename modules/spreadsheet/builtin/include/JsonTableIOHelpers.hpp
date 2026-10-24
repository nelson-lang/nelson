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
#include "Evaluator.hpp"
#include <string>
//=============================================================================
namespace Nelson::SpreadsheetGateway {
//=============================================================================
constexpr const char* NELSON_JSON_IMPORT_OPTIONS_CLASS = "nelson.io.json.JSONImportOptions";
//=============================================================================
// True when a read request (readtable, readtimetable, detectImportOptions)
// targets a JSON file: a JSONImportOptions object at 'first', a 'FileType'
// name-value set to 'json', or a '.json' extension without explicit FileType.
bool
isJsonReadRequest(const std::wstring& filename, const ArrayOfVector& argIn, size_t first);
//=============================================================================
// True when a write request (writetable, writetimetable) targets a JSON file.
bool
isJsonWriteRequest(const std::wstring& filename, const ArrayOfVector& argIn, size_t first);
//=============================================================================
// Calls the Nelson function 'name' (JSON table reader or writer) with 'args'.
ArrayOfVector
callJsonTableFunction(
    Evaluator* eval, const std::string& name, const ArrayOfVector& args, int nLhs);
//=============================================================================
} // namespace Nelson::SpreadsheetGateway
//=============================================================================
