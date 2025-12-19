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
#include "Evaluator.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
class NLSINTERPRETER_IMPEXP CallbackBase
{
protected:
    ArrayOf callbackAsArrayOf;
    bool running = false;

    bool
    executeCallbackImpl(Evaluator* eval);

public:
    CallbackBase() {};
    CallbackBase(const ArrayOf& _callbackAsArrayOf) : callbackAsArrayOf(_callbackAsArrayOf) {};
    virtual ~CallbackBase() {};

    bool
    execute(Evaluator* eval)
    {
        return executeCallbackImpl(eval);
    }

    bool
    operator==(const CallbackBase& other) const
    {
        return this->callbackAsArrayOf.getDataPointer() == other.callbackAsArrayOf.getDataPointer();
    }
};
//=============================================================================
}
//=============================================================================
