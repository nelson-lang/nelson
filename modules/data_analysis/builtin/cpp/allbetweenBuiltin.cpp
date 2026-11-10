//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "allbetweenBuiltin.hpp"
#include "objectArgumentOverload.hpp"
#include "IsBetween.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include <limits>
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::DataAnalysisGateway::allbetweenBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector overloaded;
    if (callObjectArgumentOverload(eval, nLhs, argIn, "allbetween", overloaded)) {
        return overloaded;
    }
    nargincheck(argIn, 3, std::numeric_limits<int>::max());
    nargoutcheck(nLhs, 0, 1);
    ArrayOfVector retval;
    retval << AllBetween(eval, argIn);
    return retval;
}
//=============================================================================
