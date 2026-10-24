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
#include "Operators.hpp"
#include "structBuiltin.hpp"
#include "class_subsagnBuiltin.hpp"
#include "iscellstrBuiltin.hpp"
#include "cellBuiltin.hpp"
#include "cell2matBuiltin.hpp"
#include "fieldnamesBuiltin.hpp"
#include "struct2cellBuiltin.hpp"
#include "cell2structBuiltin.hpp"
#include "cellfunBuiltin.hpp"
#include "arrayfunBuiltin.hpp"
#include "isfieldBuiltin.hpp"
#include "orderfieldsBuiltin.hpp"
#include "mat2cellBuiltin.hpp"
#include "namedargs2cellBuiltin.hpp"
#include "getfieldBuiltin.hpp"
#include "rmfieldBuiltin.hpp"
#include "num2cellBuiltin.hpp"
#include "renameStructFieldBuiltin.hpp"
#include "structfunBuiltin.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"data_structures";
//=============================================================================
static const nlsGateway gateway[] = {
    { OVERLOAD_FUNCTION_NAME(NLS_CLASS_ARRAY_STR, SUBSASGN_OPERATOR_STR),
        (ptrBuiltin)Nelson::DataStructuresGateway::class_subsagnBuiltin, 1, 3,
        CPP_BUILTIN_WITH_EVALUATOR, NLS_OVERLOAD_AUTO_OFF },
    { "isfield", (ptrBuiltin)Nelson::DataStructuresGateway::isfieldBuiltin, 1, 2 },
    { "fieldnames", (ptrBuiltin)Nelson::DataStructuresGateway::fieldnamesBuiltin, 1, 1 },
    { "cell2mat", (ptrBuiltin)Nelson::DataStructuresGateway::cell2matBuiltin, 1, 1 },
    { "orderfields", (ptrBuiltin)Nelson::DataStructuresGateway::orderfieldsBuiltin, 2, 2 },
    { "cellfun", (ptrBuiltin)Nelson::DataStructuresGateway::cellfunBuiltin, -1, -1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "arrayfun", (ptrBuiltin)Nelson::DataStructuresGateway::arrayfunBuiltin, -1, -1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "namedargs2cell", (ptrBuiltin)Nelson::DataStructuresGateway::namedargs2cellBuiltin, 1, 1 },
    { "getfield", (ptrBuiltin)Nelson::DataStructuresGateway::getfieldBuiltin, 1, 2 },
    { "rmfield", (ptrBuiltin)Nelson::DataStructuresGateway::rmfieldBuiltin, 1, 2 },
    { "struct", (ptrBuiltin)Nelson::DataStructuresGateway::structBuiltin, 1, 1 },
    { "num2cell", (ptrBuiltin)Nelson::DataStructuresGateway::num2cellBuiltin, 1, 2 },
    { "mat2cell", (ptrBuiltin)Nelson::DataStructuresGateway::mat2cellBuiltin, 1, -2, CPP_BUILTIN },
    { "iscellstr", (ptrBuiltin)Nelson::DataStructuresGateway::iscellstrBuiltin, 1, 1 },
    { "cell", (ptrBuiltin)Nelson::DataStructuresGateway::cellBuiltin, 1, 0 },
    { "struct2cell", (ptrBuiltin)Nelson::DataStructuresGateway::struct2cellBuiltin, 1, 1 },
    { "cell2struct", (ptrBuiltin)Nelson::DataStructuresGateway::cell2structBuiltin, 1, 3 },
    { "renameStructField", (ptrBuiltin)Nelson::DataStructuresGateway::renameStructFieldBuiltin, 1,
        3, CPP_BUILTIN },
    { "structfun", (ptrBuiltin)Nelson::DataStructuresGateway::structfunBuiltin, 1, -1,
        CPP_BUILTIN_WITH_EVALUATOR },
};
//=============================================================================
NLS_GATEWAY_FUNC(gateway)
//=============================================================================
NLS_GATEWAY_REMOVE(gateway)
//=============================================================================
