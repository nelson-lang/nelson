//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "NelsonGateway.hpp"
#include "readcellBuiltin.hpp"
#include "dlmreadBuiltin.hpp"
#include "dlmwriteBuiltin.hpp"
#include "writetableBuiltin.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"spreadsheet";
//=============================================================================
static const nlsGateway gateway[] = {
    { "readcell", (ptrBuiltin)Nelson::SpreadsheetGateway::readcellBuiltin, 1, 1 },
    { "dlmread", (ptrBuiltin)Nelson::SpreadsheetGateway::dlmreadBuiltin, 1, 4 },
    { "dlmwrite", (ptrBuiltin)Nelson::SpreadsheetGateway::dlmwriteBuiltin, 0, -3,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "writetable", (ptrBuiltin)Nelson::SpreadsheetGateway::writetableBuiltin, 0, 4 },
};
//=============================================================================
NLSGATEWAYFUNC(gateway)
//=============================================================================
NLSGATEWAYINFO(gateway)
//=============================================================================
NLSGATEWAYREMOVE(gateway)
//=============================================================================
NLSGATEWAYNAME()
//=============================================================================
