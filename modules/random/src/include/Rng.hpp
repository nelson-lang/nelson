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
#include "ArrayOf.hpp"
#include "Evaluator.hpp"
#include "nlsRandom_exports.h"
//=============================================================================
namespace Nelson {
NLSRANDOM_IMPEXP std::wstring
RngGetType(Evaluator* eval);
NLSRANDOM_IMPEXP void
RngSetSeed(Evaluator* eval, double seed);
NLSRANDOM_IMPEXP ArrayOf
RngGetSeed(Evaluator* eval);
NLSRANDOM_IMPEXP ArrayOf
RngGetState(Evaluator* eval);

NLSRANDOM_IMPEXP void
RngSetDefault(Evaluator* eval);
NLSRANDOM_IMPEXP void
RngShuffle(Evaluator* eval);
NLSRANDOM_IMPEXP bool
RngSetEngine(Evaluator* eval, double seed, const std::wstring& engineName);
NLSRANDOM_IMPEXP void
RngDelete(Evaluator* eval);
NLSRANDOM_IMPEXP bool
RngSetState(Evaluator* eval, const ArrayOf& st);
} // namespace Nelson
//=============================================================================
