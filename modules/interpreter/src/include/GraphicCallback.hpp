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

public:
    GraphicCallback() {};
    GraphicCallback(bool _interruptible, BUSY_ACTION _busyAction, const ArrayOf& _callbackAsArrayOf)
        : CallbackBase(_callbackAsArrayOf)
    {
        interruptible = _interruptible;
        busyAction = _busyAction;
    };
    ~GraphicCallback() {};
    bool
    isInterruptible() const;
    BUSY_ACTION
    getBusyActionState() const;
};
//=============================================================================
}
//=============================================================================
