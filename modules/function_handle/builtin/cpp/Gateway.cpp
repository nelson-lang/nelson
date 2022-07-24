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
#include "func2strBuiltin.hpp"
#include "function_handle_extractionBuiltin.hpp"
#include "function_handle_fieldnamesBuiltin.hpp"
#include "function_handle_isequalBuiltin.hpp"
#include "isfunction_handleBuiltin.hpp"
#include "str2funcBuiltin.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"function_handle";
//=============================================================================
static const nlsGateway gateway[] = {
    { "func2str", (ptrBuiltin)Nelson::FunctionHandleGateway::func2strBuiltin, 1, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "function_handle_extraction",
        (ptrBuiltin)Nelson::FunctionHandleGateway::function_handle_extractionBuiltin, -1, -1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "isfunction_handle", (ptrBuiltin)Nelson::FunctionHandleGateway::isfunction_handleBuiltin, 1,
        1 },
    { "function_handle_fieldnames",
        (ptrBuiltin)Nelson::FunctionHandleGateway::function_handle_fieldnamesBuiltin, 1, 1 },
    { "function_handle_isequal",
        (ptrBuiltin)Nelson::FunctionHandleGateway::function_handle_isequalBuiltin, 1, 2 },
    { "function_handle_isequaln",
        (ptrBuiltin)Nelson::FunctionHandleGateway::function_handle_isequalBuiltin, 1, 2 },
    { "function_handle_isequalto",
        (ptrBuiltin)Nelson::FunctionHandleGateway::function_handle_isequalBuiltin, 1, 2 },
    { "str2func", (ptrBuiltin)Nelson::FunctionHandleGateway::str2funcBuiltin, 1, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
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
