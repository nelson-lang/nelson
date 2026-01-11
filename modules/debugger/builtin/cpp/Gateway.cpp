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
#include "dbstackBuiltin.hpp"
#include "dbcontBuiltin.hpp"
#include "dbstopBuiltin.hpp"
#include "dbquitBuiltin.hpp"
#include "dbstepBuiltin.hpp"
#include "dbclearBuiltin.hpp"
#include "dbdownBuiltin.hpp"
#include "dbupBuiltin.hpp"
#include "dbstatusBuiltin.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"debugger";
//=============================================================================
static const nlsGateway gateway[] = {
    { "dbstack", (ptrBuiltin)Nelson::DebuggerGateway::dbstackBuiltin, -1, -1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "dbstop", (ptrBuiltin)Nelson::DebuggerGateway::dbstopBuiltin, 0, 4,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "dbcont", (ptrBuiltin)Nelson::DebuggerGateway::dbcontBuiltin, -1, -1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "dbquit", (ptrBuiltin)Nelson::DebuggerGateway::dbquitBuiltin, -1, -1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "dbstep", (ptrBuiltin)Nelson::DebuggerGateway::dbstepBuiltin, -1, -1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "dbclear", (ptrBuiltin)Nelson::DebuggerGateway::dbclearBuiltin, -1, -1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "dbdown", (ptrBuiltin)Nelson::DebuggerGateway::dbdownBuiltin, 0, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "dbup", (ptrBuiltin)Nelson::DebuggerGateway::dbupBuiltin, 0, 1, CPP_BUILTIN_WITH_EVALUATOR },
    { "dbstatus", (ptrBuiltin)Nelson::DebuggerGateway::dbstatusBuiltin, 0, 1,
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
