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
#include "OverloadName.hpp"
#include "pyrunBuiltin.hpp"
#include "pyrunfileBuiltin.hpp"
#include "__pyenv__Builtin.hpp"
#include "PythonEnvironment_displayBuiltin.hpp"
#include "PythonEnvironment_getBuiltin.hpp"
#include "PythonEnvironment_setBuiltin.hpp"
#include "PythonEnvironment_structBuiltin.hpp"
#include "py_displayBuiltin.hpp"
#include "py_getBuiltin.hpp"
#include "py_invokeBuiltin.hpp"
#include "py_classBuiltin.hpp"
#include "pyBuiltin.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"python_engine";
//=============================================================================
static const nlsGateway gateway[] = {
    { "pyrun", (ptrBuiltin)Nelson::Python_engineGateway::pyrunBuiltin, -1, -1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "pyrunfile", (ptrBuiltin)Nelson::Python_engineGateway::pyrunfileBuiltin, -1, -1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "__pyenv__", (ptrBuiltin)Nelson::Python_engineGateway::__pyenv__Builtin, -1, 2, CPP_BUILTIN },
    { "py", (ptrBuiltin)Nelson::Python_engineGateway::pyBuiltin, -1, 0, CPP_BUILTIN },
    { OVERLOAD_FUNCTION_NAME(NLS_HANDLE_PYTHON_ENVIRONMENT_CATEGORY_STR, "display"),
        (ptrBuiltin)Nelson::Python_engineGateway::PythonEnvironment_displayBuiltin, 0, 2,
        CPP_BUILTIN_WITH_EVALUATOR, NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_HANDLE_PYTHON_ENVIRONMENT_CATEGORY_STR, "disp"),
        (ptrBuiltin)Nelson::Python_engineGateway::PythonEnvironment_displayBuiltin, 0, 1,
        CPP_BUILTIN_WITH_EVALUATOR, NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_HANDLE_PYTHON_ENVIRONMENT_CATEGORY_STR, "get"),
        (ptrBuiltin)Nelson::Python_engineGateway::PythonEnvironment_getBuiltin, -1, 2, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_HANDLE_PYTHON_ENVIRONMENT_CATEGORY_STR, "set"),
        (ptrBuiltin)Nelson::Python_engineGateway::PythonEnvironment_setBuiltin, 0, 2, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_HANDLE_PYTHON_ENVIRONMENT_CATEGORY_STR, "struct"),
        (ptrBuiltin)Nelson::Python_engineGateway::PythonEnvironment_structBuiltin, 0, 1,
        CPP_BUILTIN, NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_HANDLE_PYOBJECT_CATEGORY_STR, "display"),
        (ptrBuiltin)Nelson::Python_engineGateway::py_displayBuiltin, 0, 2,
        CPP_BUILTIN_WITH_EVALUATOR, NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_HANDLE_PYOBJECT_CATEGORY_STR, "disp"),
        (ptrBuiltin)Nelson::Python_engineGateway::py_displayBuiltin, 0, 1,
        CPP_BUILTIN_WITH_EVALUATOR, NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_HANDLE_PYOBJECT_CATEGORY_STR, "get"),
        (ptrBuiltin)Nelson::Python_engineGateway::py_getBuiltin, 0, 2, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_HANDLE_PYOBJECT_CATEGORY_STR, "invoke"),
        (ptrBuiltin)Nelson::Python_engineGateway::py_invokeBuiltin, 0, 2, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_HANDLE_PYOBJECT_CATEGORY_STR, "class"),
        (ptrBuiltin)Nelson::Python_engineGateway::py_classBuiltin, 0, 1, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
};
//=============================================================================
static bool
initializePythonEngineModule(Nelson::Evaluator* eval)
{
    return true;
}
//=============================================================================
static bool
finishPythonEngineModule(Nelson::Evaluator* eval)
{
    return true;
}
//=============================================================================
NLSGATEWAYFUNCEXTENDED(gateway, (void*)initializePythonEngineModule)
//=============================================================================
NLSGATEWAYINFO(gateway)
//=============================================================================
NLSGATEWAYREMOVEEXTENDED(gateway, (void*)finishPythonEngineModule)
//=============================================================================
NLSGATEWAYNAME()
//=============================================================================
