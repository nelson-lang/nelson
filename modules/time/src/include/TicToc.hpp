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
#include "nlsTime_exports.h"
//=============================================================================
namespace Nelson {
NLSTIME_IMPEXP bool
Tic(Evaluator* eval);
NLSTIME_IMPEXP bool
Toc(Evaluator* eval, double& tValue);
NLSTIME_IMPEXP bool
Toc(uint64 t, double& tValue);
} // namespace Nelson
//=============================================================================
