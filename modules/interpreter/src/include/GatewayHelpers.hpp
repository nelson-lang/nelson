//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include "Evaluator.hpp"
#include "NelsonGateway.hpp"
#include "nlsInterpreter_exports.h"
//=============================================================================
using PROC_InitializeGateway = bool (*)(Nelson::Evaluator*);
using PROC_FinishGateway = bool (*)(Nelson::Evaluator*);
//=============================================================================
extern "C"
{
    NLSINTERPRETER_IMPEXP
    int
    NelsonAddGatewayWithEvaluator(void* eval, const wchar_t* moduleFilename, void* gateway,
        int nbBuiltins, const wchar_t* gatewayName, void* ptrVoidInitializeFunction);
    //=============================================================================
    NLSINTERPRETER_IMPEXP int
    NelsonRemoveGatewayWithEvaluator(void* eval, const wchar_t* moduleFilename, void* gateway,
        int nbBuiltins, const wchar_t* gatewayName, void* ptrVoidFinishFunction);
}
//=============================================================================
