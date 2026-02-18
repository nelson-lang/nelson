//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "inputnameBuiltin.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "characters_encoding.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::CoreGateway::inputnameBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 1, 1);
    Context* context = eval->getContext();
    if (context->getCurrentScope()->getName() == "base") {
        raiseError(L"Nelson:core:ERROR_INPUTNAME_NOT_IN_ACTIVE_FUNCTION",
            ERROR_INPUTNAME_NOT_IN_ACTIVE_FUNCTION);
    }
    double pos = argIn[0].getContentAsDoubleScalar();
    int ipos = (int)pos;
    if ((double)ipos != pos) {
        raiseError2(L"nelson:validators:mustBeScalarInteger", 1);
    }
    int nargin = context->getCurrentScope()->getNargIn();
    if (ipos > nargin || ipos < 1) {
        raiseError(L"Nelson:core:ERROR_ARGUMENT_NUMBER_NOT_VALID", ERROR_ARGUMENT_NUMBER_NOT_VALID);
    }
    stringVector inputNames = context->getCurrentScope()->getInputArgumentNames();
    std::string name;
    ipos = ipos - 1;
    if (ipos < inputNames.size()) {
        name = inputNames[ipos];
    }
    retval << ArrayOf::characterArrayConstructor(name);
    return retval;
}
//=============================================================================
