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
class FutureStateGuard
{
private:
    std::shared_mutex& mutex;

public:
    explicit FutureStateGuard(std::shared_mutex& m) : mutex(m) { mutex.lock_shared(); }
    ~FutureStateGuard() { mutex.unlock_shared(); }
};
//=============================================================================
NLSPARALLEL_IMPEXP
bool
WaitFutures(Evaluator* eval, const std::vector<FutureObject*>& futures, THREAD_STATE expectedState,
    double timeoutSeconds);
//=============================================================================
}
//=============================================================================
