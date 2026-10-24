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
#include "HandleState.hpp"
#include "nlsInterpreter_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
/**
 * Run the destructor protocol of one classdef handle object: notify
 * ObjectBeingDestroyed, call the user delete methods from the most derived
 * class up to its superclasses, then invalidate the handle. Runs at most once
 * per object (later calls are no-ops). With containErrors, an error raised by
 * a user delete is reported as a warning and the teardown continues.
 * Returns false when the handle is not a live classdef object.
 */
NLSINTERPRETER_IMPEXP bool
RunClassdefDelete(Evaluator* eval, const HandleRef& ref, bool containErrors);
//=============================================================================
/**
 * Finalize every object parked in PendingHandleFinalizers by the current
 * thread: each one lost its last handle reference and still owes its user
 * delete. Nested drains (a destructor releasing other objects) are allowed.
 */
NLSINTERPRETER_IMPEXP void
DrainPendingHandleFinalizers(Evaluator* eval);
//=============================================================================
/**
 * Cheap safe-point hook: drain only when something is pending.
 */
inline void
FinalizePendingHandles(Evaluator* eval)
{
    if (PendingHandleFinalizers::hasPending()) {
        DrainPendingHandleFinalizers(eval);
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
