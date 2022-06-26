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
#include "structBuiltin.hpp"
#include "iscellstrBuiltin.hpp"
#include "cellBuiltin.hpp"
#include "fieldnamesBuiltin.hpp"
#include "struct2cellBuiltin.hpp"
#include "cell2structBuiltin.hpp"
#include "cellfunBuiltin.hpp"
#include "isfieldBuiltin.hpp"
#include "namedargs2cellBuiltin.hpp"
#include "getfieldBuiltin.hpp"
#include "rmfieldBuiltin.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"data_structures";
//=============================================================================
static const nlsGateway gateway[] = {
    { "isfield", (ptrBuiltin)Nelson::DataStructuresGateway::isfieldBuiltin, 1, 2,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "fieldnames", (ptrBuiltin)Nelson::DataStructuresGateway::fieldnamesBuiltin, 1, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "cellfun", (ptrBuiltin)Nelson::DataStructuresGateway::cellfunBuiltin, -1, -1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "namedargs2cell", (ptrBuiltin)Nelson::DataStructuresGateway::namedargs2cellBuiltin, 1, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "getfield", (ptrBuiltin)Nelson::DataStructuresGateway::getfieldBuiltin, 1, 2,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "rmfield", (ptrBuiltin)Nelson::DataStructuresGateway::rmfieldBuiltin, 1, 2,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "struct", (ptrBuiltin)Nelson::DataStructuresGateway::structBuiltin, 1, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "iscellstr", (ptrBuiltin)Nelson::DataStructuresGateway::iscellstrBuiltin, 1, 1 },
    { "cell", (ptrBuiltin)Nelson::DataStructuresGateway::cellBuiltin, 1, 0 },
    { "struct2cell", (ptrBuiltin)Nelson::DataStructuresGateway::struct2cellBuiltin, 1, 1 },
    { "cell2struct", (ptrBuiltin)Nelson::DataStructuresGateway::cell2structBuiltin, 1, 3 },
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
