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
#include "nlsInterpreter_exports.h"
#include "CallbackBase.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
enum BUSY_ACTION
{
    QUEUE = 0,
    CANCEL
};
//=============================================================================
class NLSINTERPRETER_IMPEXP GraphicCallback : public CallbackBase
{
private:
    bool interruptible = true;
    BUSY_ACTION busyAction = BUSY_ACTION::QUEUE;
    int64 sourceHandle = -1;

public:
    GraphicCallback() { };
    GraphicCallback(bool _interruptible, BUSY_ACTION _busyAction, const ArrayOf& _callbackAsArrayOf,
        int64 _sourceHandle = -1)
        : CallbackBase(_callbackAsArrayOf)
    {
        interruptible = _interruptible;
        busyAction = _busyAction;
        sourceHandle = _sourceHandle;
    };
    ~GraphicCallback() { };
    bool
    execute(Evaluator* eval);
    bool
    isInterruptible() const;
    BUSY_ACTION
    getBusyActionState() const;
};
//=============================================================================
NLSINTERPRETER_IMPEXP int64
getCurrentCallbackObject();
//=============================================================================
} // namespace Nelson
//=============================================================================
