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
#include "jlrunBuiltin.hpp"
#include "jlrunfileBuiltin.hpp"
#include "__jlenv__Builtin.hpp"
#include "JuliaEnvironment_displayBuiltin.hpp"
#include "JuliaEnvironment_getBuiltin.hpp"
#include "JuliaEnvironment_setBuiltin.hpp"
#include "JuliaEnvironment_structBuiltin.hpp"
#include "jl_displayBuiltin.hpp"
#include "jl_classBuiltin.hpp"
#include "jl_ispropBuiltin.hpp"
#include "jl_getBuiltin.hpp"
#include "jl_invokeBuiltin.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"julia_engine";
//=============================================================================
static const nlsGateway gateway[] = {
    { "jlrun", (ptrBuiltin)Nelson::Julia_engineGateway::jlrunBuiltin, -1, -1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "jlrunfile", (ptrBuiltin)Nelson::Julia_engineGateway::jlrunfileBuiltin, -1, -1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "__jlenv__", (ptrBuiltin)Nelson::Julia_engineGateway::__jlenv__Builtin, -1, 2, CPP_BUILTIN },
    { OVERLOAD_FUNCTION_NAME(NLS_HANDLE_JULIA_CATEGORY_STR, "display"),
        (ptrBuiltin)Nelson::Julia_engineGateway::jl_displayBuiltin, 0, 2,
        CPP_BUILTIN_WITH_EVALUATOR, NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_HANDLE_JULIA_CATEGORY_STR, "disp"),
        (ptrBuiltin)Nelson::Julia_engineGateway::jl_displayBuiltin, 0, 1,
        CPP_BUILTIN_WITH_EVALUATOR, NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_HANDLE_JULIA_CATEGORY_STR, "class"),
        (ptrBuiltin)Nelson::Julia_engineGateway::jl_classBuiltin, -1, 1, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_HANDLE_JULIA_CATEGORY_STR, "get"),
        (ptrBuiltin)Nelson::Julia_engineGateway::jl_getBuiltin, 0, 2, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_HANDLE_JULIA_CATEGORY_STR, "isprop"),
        (ptrBuiltin)Nelson::Julia_engineGateway::jl_ispropBuiltin, 0, 2, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_HANDLE_JULIA_CATEGORY_STR, "invoke"),
        (ptrBuiltin)Nelson::Julia_engineGateway::jl_invokeBuiltin, 0, 2, CPP_BUILTIN_WITH_EVALUATOR,
        NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_HANDLE_JULIA_ENVIRONMENT_CATEGORY_STR, "display"),
        (ptrBuiltin)Nelson::Julia_engineGateway::JuliaEnvironment_displayBuiltin, 0, 2,
        CPP_BUILTIN_WITH_EVALUATOR, NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_HANDLE_JULIA_ENVIRONMENT_CATEGORY_STR, "disp"),
        (ptrBuiltin)Nelson::Julia_engineGateway::JuliaEnvironment_displayBuiltin, 0, 1,
        CPP_BUILTIN_WITH_EVALUATOR, NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_HANDLE_JULIA_ENVIRONMENT_CATEGORY_STR, "get"),
        (ptrBuiltin)Nelson::Julia_engineGateway::JuliaEnvironment_getBuiltin, -1, 2, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_HANDLE_JULIA_ENVIRONMENT_CATEGORY_STR, "set"),
        (ptrBuiltin)Nelson::Julia_engineGateway::JuliaEnvironment_setBuiltin, 0, 2, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_HANDLE_JULIA_ENVIRONMENT_CATEGORY_STR, "struct"),
        (ptrBuiltin)Nelson::Julia_engineGateway::JuliaEnvironment_structBuiltin, 0, 1, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
};
//=============================================================================
static bool
initializeJuliaEngineModule(Nelson::Evaluator* eval)
{
    return true;
}
//=============================================================================
static bool
finishJuliaEngineModule(Nelson::Evaluator* eval)
{
    return true;
}
//=============================================================================
NLSGATEWAYFUNCEXTENDED(gateway, (void*)initializeJuliaEngineModule)
//=============================================================================
NLSGATEWAYINFO(gateway)
//=============================================================================
NLSGATEWAYREMOVEEXTENDED(gateway, (void*)finishJuliaEngineModule)
//=============================================================================
NLSGATEWAYNAME()
//=============================================================================
