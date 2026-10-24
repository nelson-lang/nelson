//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "narginchkBuiltin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::CoreGateway::narginchkBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 0);
    nargincheck(argIn, 2, 2);
    Context* context = eval->getContext();
    if (context->getCurrentScope()->getName() == "base") {
        Error(L"Nelson:core:notInFunction",
            L"You can only call '%s' from within a Nelson function.", L"narginchk");
    }
    if (!argIn[0].isScalar() || !argIn[0].isNumeric()) {
        Error("Nelson:expectedScalar", "%s must be a scalar.", "Argument #1");
    }
    if (!argIn[1].isScalar() || !argIn[1].isNumeric()) {
        Error("Nelson:expectedScalar", "%s must be a scalar.", "Argument #2");
    }
    int minArgs = argIn[0].asInt32Scalar(false, true);
    bool maxArgsIsInf = false;
    if (argIn[1].isDoubleType(true)) {
        double maxValue = argIn[1].asDoubleScalar();
        maxArgsIsInf = std::isinf(maxValue);
    }
    int nargin = context->getCurrentScope()->getNargIn();
    if (nargin < minArgs) {
        ErrorAsCaller(L"Nelson:narginchk:notEnoughInputs", L"Not enough input arguments.");
    }
    if (!maxArgsIsInf) {
        int maxArgs = argIn[1].asInt32Scalar(false, true);

        if (nargin > maxArgs) {
            ErrorAsCaller(L"Nelson:narginchk:tooManyInputs", L"Too many input arguments.");
        }
    }
    return retval;
}
//=============================================================================
