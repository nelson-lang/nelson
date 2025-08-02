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
#include "Operators.hpp"
#include "OverloadName.hpp"
#include "func2strBuiltin.hpp"
#include "function_handle_extractionBuiltin.hpp"
#include "function_handle_fieldnamesBuiltin.hpp"
#include "function_handle_isequalBuiltin.hpp"
#include "isfunction_handleBuiltin.hpp"
#include "str2funcBuiltin.hpp"
#include "function_handle_displayBuiltin.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"function_handle";
//=============================================================================
static const nlsGateway gateway[] = {
    { OVERLOAD_FUNCTION_NAME(NLS_FUNCTION_HANDLE_STR, SUBSREF_OPERATOR_STR),
        (ptrBuiltin)Nelson::FunctionHandleGateway::function_handle_extractionBuiltin, -1, -1,
        CPP_BUILTIN_WITH_EVALUATOR, NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_FUNCTION_HANDLE_STR, "fieldnames"),
        (ptrBuiltin)Nelson::FunctionHandleGateway::function_handle_fieldnamesBuiltin, 1, 1,
        CPP_BUILTIN, NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_FUNCTION_HANDLE_STR, "isequal"),
        (ptrBuiltin)Nelson::FunctionHandleGateway::function_handle_isequalBuiltin, 1, 2,
        CPP_BUILTIN, NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_FUNCTION_HANDLE_STR, "isequaln"),
        (ptrBuiltin)Nelson::FunctionHandleGateway::function_handle_isequalBuiltin, 1, 2,
        CPP_BUILTIN, NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_FUNCTION_HANDLE_STR, "isequalto"),
        (ptrBuiltin)Nelson::FunctionHandleGateway::function_handle_isequalBuiltin, 1, 2,
        CPP_BUILTIN, NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_FUNCTION_HANDLE_STR, "display"),
        (ptrBuiltin)Nelson::FunctionHandleGateway::function_handle_displayBuiltin, 0, 2,
        CPP_BUILTIN_WITH_EVALUATOR, NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_FUNCTION_HANDLE_STR, "disp"),
        (ptrBuiltin)Nelson::FunctionHandleGateway::function_handle_dispBuiltin, 0, 1,
        CPP_BUILTIN_WITH_EVALUATOR, NLS_OVERLOAD_AUTO_OFF },
    //=============================================================================
    { "str2func", (ptrBuiltin)Nelson::FunctionHandleGateway::str2funcBuiltin, 1, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "func2str", (ptrBuiltin)Nelson::FunctionHandleGateway::func2strBuiltin, 1, 1 },
    { "isfunction_handle", (ptrBuiltin)Nelson::FunctionHandleGateway::isfunction_handleBuiltin, 1,
        1 },
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
