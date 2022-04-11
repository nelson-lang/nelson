//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include "Evaluator.hpp"
#include "nlsInterpreter_exports.h"
//=============================================================================
namespace Nelson {
NLSINTERPRETER_IMPEXP void
setWarningEvaluator(Evaluator* eval);
}
//=============================================================================
#ifdef __cplusplus
extern "C"
{
#endif
    //=============================================================================
    NLSINTERPRETER_IMPEXP void
    NelsonWarningEmitter(const wchar_t* msg, const wchar_t* id, bool asError);
//=============================================================================
#ifdef __cplusplus
}
#endif
//=============================================================================
