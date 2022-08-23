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
#include <vector>
#include "nlsParallel_exports.h"
#include "Evaluator.hpp"
#include "FutureObject.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
NLSPARALLEL_IMPEXP
ArrayOfVector
FevalFutureFetchNext(
    Evaluator* eval, const std::vector<FutureObject*>& futures, int nLhs, double timeout);
//=============================================================================
}
//=============================================================================
