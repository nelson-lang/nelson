//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "max_recursion_depthBuiltin.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "PredefinedErrorMessages.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::InterpreterGateway::max_recursion_depthBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);
    Context* context = eval->getContext();
    if (argIn.size() == 0) {
        size_t recursiondepth = context->getRecursionDepth();
        retval << ArrayOf::doubleConstructor((double)recursiondepth);
    } else if (argIn.size() == 1) {
        size_t previousrecursiondepth = context->getRecursionDepth();
        ArrayOf param1 = argIn[0];
        if (param1.isRowVectorCharacterArray()) {
            std::wstring param = param1.getContentAsWideString();
            if (param == L"max") {
                context->setRecursionDepth(context->getMaximumRecursionDepth());
            } else {
                Error(_W("Argument #1: 'max' expected."));
            }
        } else {
            indexType value = param1.getContentAsScalarIndex();
            if (value <= (indexType)context->getMaximumRecursionDepth()) {
                context->setRecursionDepth((size_t)value);
            } else {
                Error(_W("Argument #1: valid value expected."));
            }
        }
        retval << ArrayOf::doubleConstructor((double)previousrecursiondepth);
    } else {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    return retval;
}
//=============================================================================
