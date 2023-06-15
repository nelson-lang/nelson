//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
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
#include "__num2cell__Builtin.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"data_structures";
//=============================================================================
static const nlsGateway gateway[] = {
    { "isfield", (ptrBuiltin)Nelson::DataStructuresGateway::isfieldBuiltin, 1, 2, CPP_BUILTIN },
    { OVERLOAD_METHOD_NAME(NLS_CELL_ARRAY_STR, "isfield"),
        (ptrBuiltin)Nelson::DataStructuresGateway::isfieldBuiltin, 1, 2, CPP_BUILTIN },
    { "fieldnames", (ptrBuiltin)Nelson::DataStructuresGateway::fieldnamesBuiltin, 1, 1,
        CPP_BUILTIN },
    { OVERLOAD_METHOD_NAME(NLS_STRUCT_ARRAY_STR, "fieldnames"),
        (ptrBuiltin)Nelson::DataStructuresGateway::fieldnamesBuiltin, 1, 1, CPP_BUILTIN },
    { "cellfun", (ptrBuiltin)Nelson::DataStructuresGateway::cellfunBuiltin, -1, -1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "namedargs2cell", (ptrBuiltin)Nelson::DataStructuresGateway::namedargs2cellBuiltin, 1, 1,
        CPP_BUILTIN },
    { OVERLOAD_METHOD_NAME(NLS_STRUCT_ARRAY_STR, "namedargs2cell"),
        (ptrBuiltin)Nelson::DataStructuresGateway::namedargs2cellBuiltin, 1, 1, CPP_BUILTIN },
    { "getfield", (ptrBuiltin)Nelson::DataStructuresGateway::getfieldBuiltin, 1, 2, CPP_BUILTIN },
    { OVERLOAD_METHOD_NAME(NLS_STRUCT_ARRAY_STR, "getfield"),
        (ptrBuiltin)Nelson::DataStructuresGateway::getfieldBuiltin, 1, 2, CPP_BUILTIN },
    { "rmfield", (ptrBuiltin)Nelson::DataStructuresGateway::rmfieldBuiltin, 1, 2, CPP_BUILTIN },
    { OVERLOAD_METHOD_NAME(NLS_STRUCT_ARRAY_STR, "rmfield"),
        (ptrBuiltin)Nelson::DataStructuresGateway::rmfieldBuiltin, 1, 2, CPP_BUILTIN },
    { "struct", (ptrBuiltin)Nelson::DataStructuresGateway::structBuiltin, 1, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "__num2cell__", (ptrBuiltin)Nelson::DataStructuresGateway::__num2cell__Builtin, 1, 1,
        CPP_BUILTIN },
    { "iscellstr", (ptrBuiltin)Nelson::DataStructuresGateway::iscellstrBuiltin, 1, 1 },
    { OVERLOAD_METHOD_NAME(NLS_CELL_ARRAY_STR, "iscellstr"),
        (ptrBuiltin)Nelson::DataStructuresGateway::iscellstrBuiltin, 1, 1 },
    { "cell", (ptrBuiltin)Nelson::DataStructuresGateway::cellBuiltin, 1, 0 },
    { "struct2cell", (ptrBuiltin)Nelson::DataStructuresGateway::struct2cellBuiltin, 1, 1 },
    { OVERLOAD_METHOD_NAME(NLS_STRUCT_ARRAY_STR, "struct2cell"),
        (ptrBuiltin)Nelson::DataStructuresGateway::struct2cellBuiltin, 1, 1 },
    { "cell2struct", (ptrBuiltin)Nelson::DataStructuresGateway::cell2structBuiltin, 1, 3 },
    { OVERLOAD_METHOD_NAME(NLS_CELL_ARRAY_STR, "cell2struct"),
        (ptrBuiltin)Nelson::DataStructuresGateway::cell2structBuiltin, 1, 1 },
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
