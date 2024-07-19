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
#include "nlsInterpreter_exports.h"
#include "Evaluator.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
enum BUSY_ACTION
{
    QUEUE = 0,
    CANCEL
};
//=============================================================================
class NLSINTERPRETER_IMPEXP GraphicCallback
{
private:
    ArrayOf callbackAsArrayOf;
    bool interruptible = true;

    bool running = false;
    BUSY_ACTION busyAction = BUSY_ACTION::QUEUE;

public:
    GraphicCallback() {};
    GraphicCallback(bool _interruptible, BUSY_ACTION _busyAction, ArrayOf& _callbackAsArrayOf)
        : callbackAsArrayOf(_callbackAsArrayOf)
    {
        interruptible = _interruptible;
        busyAction = _busyAction;
    };
    ~GraphicCallback() {};
    bool
    isInterruptible();
    bool
    execute(Evaluator* eval);
    BUSY_ACTION
    getBusyActionState();
    bool
    operator==(const GraphicCallback& other) const
    {
        return this->callbackAsArrayOf.getDataPointer() == other.callbackAsArrayOf.getDataPointer();
    }
};
//=============================================================================
}
//=============================================================================
