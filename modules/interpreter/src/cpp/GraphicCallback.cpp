//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "GraphicCallback.hpp"
#include <vector>
//=============================================================================
namespace Nelson {
//=============================================================================
static std::vector<int64> callbackObjectStack;
//=============================================================================
bool
GraphicCallback::execute(Evaluator* eval)
{
    callbackObjectStack.push_back(sourceHandle);
    bool result = executeCallbackImpl(eval);
    callbackObjectStack.pop_back();
    return result;
}
//=============================================================================
bool
GraphicCallback::isInterruptible() const
{
    return interruptible;
}
//=============================================================================
BUSY_ACTION
GraphicCallback::getBusyActionState() const { return busyAction; }
//=============================================================================
int64
getCurrentCallbackObject()
{
    if (callbackObjectStack.empty()) {
        return -1;
    }
    return callbackObjectStack.back();
}
//=============================================================================
} // namespace Nelson
//=============================================================================
