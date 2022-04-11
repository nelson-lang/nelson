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
#include "NargIn.hpp"
#include "Validators.hpp"
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
    mustBeScalarOrEmpty(argIn, 0);
    mustBeNonempty(argIn, 0);
    mustBeInteger(argIn, 0);

    mustBeScalarOrEmpty(argIn, 1);
    mustBeNonempty(argIn, 1);
    mustBeInteger(argIn, 1);

    int minArgs = argIn[0].getContentAsInteger32Scalar();
    int maxArgs = argIn[1].getContentAsInteger32Scalar();

    int nargin = context->getCurrentScope()->getNargIn();
    if (nargin < minArgs) {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS, L"Nelson:narginchk:notEnoughInputs", true);
    }
    if (nargin > maxArgs) {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS, L"Nelson:narginchk:tooManyInputs", true);
    }
    return retval;
}
//=============================================================================
