//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "NelsonGateway.hpp"
#include "readcellBuiltin.hpp"
#include "readmatrixBuiltin.hpp"
#include "readtableBuiltin.hpp"
#include "readtimetableBuiltin.hpp"
#include "dlmreadBuiltin.hpp"
#include "dlmwriteBuiltin.hpp"
#include "writetableBuiltin.hpp"
#include "writetimetableBuiltin.hpp"
#include "detectImportOptionsBuiltin.hpp"
#include "xlsfinfoBuiltin.hpp"
#include "__write_json_table__Builtin.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"spreadsheet";
//=============================================================================
static const nlsGateway gateway[] = {
    { "readcell", (ptrBuiltin)Nelson::SpreadsheetGateway::readcellBuiltin, 1, 1 },
    { "readmatrix", (ptrBuiltin)Nelson::SpreadsheetGateway::readmatrixBuiltin, 1, 1 },
    { "readtable", (ptrBuiltin)Nelson::SpreadsheetGateway::readtableBuiltin, 1, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "readtimetable", (ptrBuiltin)Nelson::SpreadsheetGateway::readtimetableBuiltin, 1, -1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "dlmread", (ptrBuiltin)Nelson::SpreadsheetGateway::dlmreadBuiltin, 1, 4 },
    { "dlmwrite", (ptrBuiltin)Nelson::SpreadsheetGateway::dlmwriteBuiltin, 0, -3,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "writetable", (ptrBuiltin)Nelson::SpreadsheetGateway::writetableBuiltin, 0, 4,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "writetimetable", (ptrBuiltin)Nelson::SpreadsheetGateway::writetimetableBuiltin, 0, -1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "detectImportOptions", (ptrBuiltin)Nelson::SpreadsheetGateway::detectImportOptionsBuiltin, 1,
        -1, CPP_BUILTIN_WITH_EVALUATOR },
    { "xlsfinfo", (ptrBuiltin)Nelson::SpreadsheetGateway::xlsfinfoBuiltin, -1, 1 },
    { "__write_json_table__", (ptrBuiltin)Nelson::SpreadsheetGateway::__write_json_table__Builtin,
        0, 5 },
};
//=============================================================================
NLS_GATEWAY_FUNC(gateway)
//=============================================================================
NLS_GATEWAY_REMOVE(gateway)
//=============================================================================
