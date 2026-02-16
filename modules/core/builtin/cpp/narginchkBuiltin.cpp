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
        raiseError(L"Nelson:core:ERROR_NARGINCHK_ONLY_FROM_NELSON_FUNCTION",
            ERROR_NARGINCHK_ONLY_FROM_NELSON_FUNCTION);
    }
    if (!argIn[0].isScalar() || !argIn[0].isNumeric()) {
        raiseError(L"Nelson:core:ERROR_NARGINCHK_SCALAR_INTEGER_REQUIRED_ARG1",
            ERROR_NARGINCHK_SCALAR_INTEGER_REQUIRED_ARG1);
    }
    if (!argIn[1].isScalar() || !argIn[1].isNumeric()) {
        raiseError(L"Nelson:core:ERROR_NARGINCHK_SCALAR_INTEGER_REQUIRED_ARG2",
            ERROR_NARGINCHK_SCALAR_INTEGER_REQUIRED_ARG2);
    }
    int minArgs = argIn[0].getContentAsInteger32Scalar(false, true);
    bool maxArgsIsInf = false;
    if (argIn[1].isDoubleType(true)) {
        double maxValue = argIn[1].getContentAsDoubleScalar();
        maxArgsIsInf = std::isinf(maxValue);
    }
    int nargin = context->getCurrentScope()->getNargIn();
    if (nargin < minArgs) {
        raiseErrorAsCaller(true, L"nelson:arguments:tooFewInputs");
    }
    if (!maxArgsIsInf) {
        int maxArgs = argIn[1].getContentAsInteger32Scalar(false, true);

        if (nargin > maxArgs) {
            raiseErrorAsCaller(true, L"nelson:arguments:tooManyInputs");
        }
    }
    return retval;
}
//=============================================================================
