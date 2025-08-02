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
#include <vector>
#include "nlsParallel_exports.h"
#include "Evaluator.hpp"
#include "FutureObject.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
NLSPARALLEL_IMPEXP
ArrayOfVector
FutureFetchOutputs(
    Evaluator* eval, const std::vector<FutureObject*>& futures, bool uniformOutput = false);
//=============================================================================
}
//=============================================================================
