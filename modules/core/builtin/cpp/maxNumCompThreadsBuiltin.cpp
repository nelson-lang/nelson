//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "maxNumCompThreadsBuiltin.hpp"
#include "ComputionalThreads.hpp"
#include "NelsonConfiguration.hpp"
#include "Error.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "PredefinedErrorMessages.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::CoreGateway::maxNumCompThreadsBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);
    unsigned int currentValue = NelsonConfiguration::getInstance()->getMaxNumCompThreads();
    switch (argIn.size()) {
    case 0: {
        retval << ArrayOf::doubleConstructor(static_cast<double>(currentValue));
    } break;
    case 1: {
        ArrayOf param1 = argIn[0];
        if (param1.isRowVectorCharacterArray()) {
            std::wstring str = param1.getContentAsWideString();
            if (str == L"automatic") {
                setDefaultMaxNumCompThreads();
            } else {
                Error(ERROR_WRONG_ARGUMENT_1_VALUE);
            }
        } else {
            indexType N = param1.getContentAsScalarIndex(false);
            setMaxNumCompThreads(static_cast<unsigned int>(N));
        }
        retval << ArrayOf::doubleConstructor(static_cast<double>(currentValue));
    } break;
    default: {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    } break;
    }
    return retval;
}
//=============================================================================
