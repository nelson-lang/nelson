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
    { OVERLOAD_FUNCTION_NAME("MException", "fieldnames"),
        (ptrBuiltin)Nelson::ErrorManagerGateway::MException_fieldnamesBuiltin, 1, 1, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    //=============================================================================
    { "error", (ptrBuiltin)Nelson::ErrorManagerGateway::errorBuiltin, 0, 2,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "warning", (ptrBuiltin)Nelson::ErrorManagerGateway::warningBuiltin, 1, -1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "lasterror", (ptrBuiltin)Nelson::ErrorManagerGateway::lasterrorBuiltin, 1, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "lastwarn", (ptrBuiltin)Nelson::ErrorManagerGateway::lastwarnBuiltin, 2, 2,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "getLastReport", (ptrBuiltin)Nelson::ErrorManagerGateway::getLastReportBuiltin, 1, 0,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "MException", (ptrBuiltin)Nelson::ErrorManagerGateway::MExceptionBuiltin, 1, 2,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "throw", (ptrBuiltin)Nelson::ErrorManagerGateway::throwBuiltin, 0, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "throwAsCaller", (ptrBuiltin)Nelson::ErrorManagerGateway::throwAsCallerBuiltin, 0, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "rethrow", (ptrBuiltin)Nelson::ErrorManagerGateway::rethrowBuiltin, 0, 1, CPP_BUILTIN },
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
