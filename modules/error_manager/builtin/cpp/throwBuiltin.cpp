//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "throwBuiltin.hpp"
#include "MException.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "ClassName.hpp"
#include "DebugStack.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::ErrorManagerGateway::throwBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 0);
    if (argIn[0].isClassType() && ClassName(argIn[0]) == "MException") {
        if (argIn[0].isScalar()) {
            Exception mException = ArrayOfToException(argIn[0]);
            Nelson::stackTrace trace;
            DebugStack(eval->callstack, 1, trace);
            mException.setTrace(trace);
            throw mException;
        }
        Error(_W("MException scalar expected."));
    }
    Error(_W("MException expected."));
    ArrayOfVector retval;
    return retval;
}
//=============================================================================
