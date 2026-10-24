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
#include "mexextBuiltin.hpp"
#include "FunctionsInMemory.hpp"
#include "MxMemory.h"
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"mex";
//=============================================================================
static const nlsGateway gateway[]
    = { { "mexext", (ptrBuiltin)Nelson::MexGateway::mexextBuiltin, 1, 1, CPP_BUILTIN } };
//=============================================================================
static bool
initializeMexModule(Nelson::Evaluator* eval)
{
    (void)eval;
    // Hand the core (FunctionsInMemory) our persistent-memory cleanup so
    // 'clear mex' / 'clear all' can free it without memory_manager (an
    // always-installed module) linking this optional mex module.
    FunctionsInMemory::getInstance()->registerMexMemoryCleanup(mexFreeAllRegisteredPointer);
    return true;
}
//=============================================================================
static bool
finishMexModule(Nelson::Evaluator* eval)
{
    (void)eval;
    // Drop the callback so the core never calls into this module after it is
    // unloaded.
    FunctionsInMemory::getInstance()->registerMexMemoryCleanup(nullptr);
    return true;
}
//=============================================================================
NLS_GATEWAY_FUNC_EXTENDED(gateway, (void*)initializeMexModule)
//=============================================================================
NLS_GATEWAY_REMOVE_EXTENDED(gateway, (void*)finishMexModule)
//=============================================================================
