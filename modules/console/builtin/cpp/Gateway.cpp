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
#include "clcBuiltin.hpp"
#include "inputBuiltin.hpp"
#include "terminal_sizeBuiltin.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"console";
//=============================================================================
static const nlsGateway gateway[] = {
    { "clc", (ptrBuiltin)Nelson::ConsoleGateway::clcBuiltin, 0, 0, CPP_BUILTIN_WITH_EVALUATOR },
    { "input", (ptrBuiltin)Nelson::ConsoleGateway::inputBuiltin, 1, 1, CPP_BUILTIN_WITH_EVALUATOR },
    { "terminal_size", (ptrBuiltin)Nelson::ConsoleGateway::terminal_sizeBuiltin, 1, 0,
        CPP_BUILTIN_WITH_EVALUATOR }

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
