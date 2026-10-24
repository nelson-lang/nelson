//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#ifdef _MSC_VER
#pragma warning(disable : 4190)
#endif
//=============================================================================
#include "NelsonGateway.hpp"
#include "ClassdefParser.hpp"
#include "MException.hpp"
#include "OverloadName.hpp"
#include "errorBuiltin.hpp"
#include "lasterrorBuiltin.hpp"
#include "lastwarnBuiltin.hpp"
#include "warningBuiltin.hpp"
#include "getLastReportBuiltin.hpp"
#include "MException_fieldnamesBuiltin.hpp"
#include "MExceptionBuiltin.hpp"
#include "throwBuiltin.hpp"
#include "throwAsCallerBuiltin.hpp"
#include "rethrowBuiltin.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"error_manager";
//=============================================================================
static const nlsGateway gateway[] = {
    { "MException.last", (ptrBuiltin)Nelson::ErrorManagerGateway::MException_lastBuiltin, 1, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { OVERLOAD_FUNCTION_NAME("MException", "fieldnames"),
        (ptrBuiltin)Nelson::ErrorManagerGateway::MException_fieldnamesBuiltin, 1, 1, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME("MException", "properties"),
        (ptrBuiltin)Nelson::ErrorManagerGateway::MException_propertiesBuiltin, 1, 1, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME("MException", "methods"),
        (ptrBuiltin)Nelson::ErrorManagerGateway::MException_methodsBuiltin, 1, 1, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME("MException", "subsasgn"),
        (ptrBuiltin)Nelson::ErrorManagerGateway::MException_subsasgnBuiltin, 1, 3, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME("MException", "addCause"),
        (ptrBuiltin)Nelson::ErrorManagerGateway::MException_addCauseBuiltin, 1, 2, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME("MException", "addCorrection"),
        (ptrBuiltin)Nelson::ErrorManagerGateway::MException_addCorrectionBuiltin, 1, 2, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME("MException", "getReport"),
        (ptrBuiltin)Nelson::ErrorManagerGateway::MException_getReportBuiltin, 1, 4, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME("MException", "eq"),
        (ptrBuiltin)Nelson::ErrorManagerGateway::MException_eqBuiltin, 1, 2, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME("MException", "ne"),
        (ptrBuiltin)Nelson::ErrorManagerGateway::MException_neBuiltin, 1, 2, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME("MException", "isequal"),
        (ptrBuiltin)Nelson::ErrorManagerGateway::MException_isequalBuiltin, 1, -1, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME("MException", "isequaln"),
        (ptrBuiltin)Nelson::ErrorManagerGateway::MException_isequalBuiltin, 1, -1, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME("MException", "isequalto"),
        (ptrBuiltin)Nelson::ErrorManagerGateway::MException_isequalBuiltin, 1, -1, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME("MException", "throw"),
        (ptrBuiltin)Nelson::ErrorManagerGateway::throwBuiltin, 0, 1, CPP_BUILTIN_WITH_EVALUATOR,
        NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME("MException", "throwAsCaller"),
        (ptrBuiltin)Nelson::ErrorManagerGateway::throwAsCallerBuiltin, 0, 1,
        CPP_BUILTIN_WITH_EVALUATOR, NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME("MException", "rethrow"),
        (ptrBuiltin)Nelson::ErrorManagerGateway::rethrowBuiltin, 0, 1, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    //=============================================================================
    { "error", (ptrBuiltin)Nelson::ErrorManagerGateway::errorBuiltin, 0, -1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "warning", (ptrBuiltin)Nelson::ErrorManagerGateway::warningBuiltin, 1, -1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "lasterror", (ptrBuiltin)Nelson::ErrorManagerGateway::lasterrorBuiltin, 1, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "lastwarn", (ptrBuiltin)Nelson::ErrorManagerGateway::lastwarnBuiltin, 2, 2,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "getLastReport", (ptrBuiltin)Nelson::ErrorManagerGateway::getLastReportBuiltin, 1, 0,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "addCause", (ptrBuiltin)Nelson::ErrorManagerGateway::MException_addCauseBuiltin, 1, 2,
        CPP_BUILTIN },
    { "addCorrection", (ptrBuiltin)Nelson::ErrorManagerGateway::MException_addCorrectionBuiltin, 1,
        2, CPP_BUILTIN },
    { "getReport", (ptrBuiltin)Nelson::ErrorManagerGateway::MException_getReportBuiltin, 1, 4,
        CPP_BUILTIN },
    { "MException", (ptrBuiltin)Nelson::ErrorManagerGateway::MExceptionBuiltin, 1, -1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "throw", (ptrBuiltin)Nelson::ErrorManagerGateway::throwBuiltin, 0, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "throwAsCaller", (ptrBuiltin)Nelson::ErrorManagerGateway::throwAsCallerBuiltin, 0, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "rethrow", (ptrBuiltin)Nelson::ErrorManagerGateway::rethrowBuiltin, 0, 1, CPP_BUILTIN },
};
//=============================================================================
static bool
initializeErrorManagerModule(Evaluator*)
{
    classdefRegisterBuiltinClass(MEXCEPTION_CLASS_NAME, MExceptionPropertyNames(),
        MExceptionMethodNames(), { "last" }, true);
    return true;
}
//=============================================================================
NLS_GATEWAY_FUNC_EXTENDED(gateway, (void*)initializeErrorManagerModule)
//=============================================================================
NLS_GATEWAY_REMOVE(gateway)
//=============================================================================
