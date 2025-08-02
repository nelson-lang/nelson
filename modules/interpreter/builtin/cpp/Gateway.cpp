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
#include "iskeywordBuiltin.hpp"
#include "max_recursion_depthBuiltin.hpp"
#include "parsefileBuiltin.hpp"
#include "parsestringBuiltin.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"interpreter";
//=============================================================================
static const nlsGateway gateway[] = {
    { "parsefile", (ptrBuiltin)Nelson::InterpreterGateway::parsefileBuiltin, 1, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "parsestring", (ptrBuiltin)Nelson::InterpreterGateway::parsestringBuiltin, 1, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "max_recursion_depth", (ptrBuiltin)Nelson::InterpreterGateway::max_recursion_depthBuiltin, 1,
        1, CPP_BUILTIN_WITH_EVALUATOR },
    { "iskeyword", (ptrBuiltin)Nelson::InterpreterGateway::iskeywordBuiltin, 1, 1 },
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
