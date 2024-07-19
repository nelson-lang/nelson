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
class NLSINTERPRETER_IMPEXP GraphicCallback
{
private:
    ArrayOf callbackAsArrayOf;
    bool running = false;

public:
    GraphicCallback() {};
    GraphicCallback(const ArrayOf& _callbackAsArrayOf) : callbackAsArrayOf(_callbackAsArrayOf) {};
    ~GraphicCallback() {};

    bool
    execute(Evaluator* eval);
};
//=============================================================================
}
//=============================================================================
