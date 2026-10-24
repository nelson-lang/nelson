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
#include "ErrorID.hpp"
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
        Error(L"Nelson:inputname:notInAfunction",
            L"Cannot return input name if not in an active function.");
    }
    double pos = argIn[0].asDoubleScalar();
    int ipos = (int)pos;
    if ((double)ipos != pos) {
        Error(L"Nelson:IntVal", L"Scalar integer value required, but value is not integral.");
    }
    int nargin = context->getCurrentScope()->getNargIn();
    if (ipos > nargin || ipos < 1) {
        Error(L"Nelson:inputname:argNumberNotValid", L"Argument number is not valid.");
    }
    stringVector inputNames = context->getCurrentScope()->getInputArgumentNames();
    std::string name;
    ipos = ipos - 1;
    if (ipos < inputNames.size()) {
        name = inputNames[ipos];
    }
    retval << ArrayOf::characterArray(name);
    return retval;
}
//=============================================================================
