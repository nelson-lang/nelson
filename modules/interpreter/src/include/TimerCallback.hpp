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
class NLSINTERPRETER_IMPEXP TimerCallback
{
private:
    ArrayOf callbackAsArrayOf;
    bool running = false;

public:
    TimerCallback() {};
    TimerCallback(const ArrayOf& _callbackAsArrayOf) : callbackAsArrayOf(_callbackAsArrayOf) {};
    ~TimerCallback() {};
    bool
    execute(Evaluator* eval);
    bool
    operator==(const TimerCallback& other) const
    {
        return this->callbackAsArrayOf.getDataPointer() == other.callbackAsArrayOf.getDataPointer();
    }
};
//=============================================================================
}
//=============================================================================
