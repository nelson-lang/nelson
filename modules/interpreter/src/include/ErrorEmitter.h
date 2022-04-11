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
#include "Exception.hpp"
#include "nlsInterpreter_exports.h"
//=============================================================================
namespace Nelson {
NLSINTERPRETER_IMPEXP void
setErrorEvaluator(Evaluator* eval);
NLSINTERPRETER_IMPEXP void
throwException(const Exception& e);
} // namespace Nelson
//=============================================================================
#ifdef __cplusplus
extern "C"
{
#endif
    //=============================================================================
    NLSINTERPRETER_IMPEXP void
    NelsonErrorEmitter(const wchar_t* msg, const wchar_t* id, bool asCaller);
//=============================================================================
#ifdef __cplusplus
}
#endif
//=============================================================================
