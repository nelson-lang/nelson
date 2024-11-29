//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
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
        Error(_W("You can only call 'narginchk' from within a Nelson function."));
    }
    if (!argIn[0].isScalar() || !argIn[0].isNumeric()) {
        Error(_("Scalar integer value required for #1 argument."));
    }
    if (!argIn[1].isScalar() || !argIn[1].isNumeric()) {
        Error(_("Scalar integer value required for #2 argument."));
    }
    int minArgs = argIn[0].getContentAsInteger32Scalar(false, true);
    bool maxArgsIsInf = false;
    if (argIn[1].isDoubleType(true)) {
        double maxValue = argIn[1].getContentAsDoubleScalar();
        maxArgsIsInf = std::isinf(maxValue);
    }
    int nargin = context->getCurrentScope()->getNargIn();
    if (nargin < minArgs) {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS, L"Nelson:narginchk:notEnoughInputs", true);
    }
    if (!maxArgsIsInf) {
        int maxArgs = argIn[1].getContentAsInteger32Scalar(false, true);

        if (nargin > maxArgs) {
            Error(ERROR_WRONG_NUMBERS_INPUT_ARGS, L"Nelson:narginchk:tooManyInputs", true);
        }
    }
    return retval;
}
//=============================================================================
