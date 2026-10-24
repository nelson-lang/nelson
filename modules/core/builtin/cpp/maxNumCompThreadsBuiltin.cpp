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
#include "ErrorID.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::CoreGateway::maxNumCompThreadsBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);
    unsigned int currentValue = NelsonConfiguration::getInstance()->maxNumCompThreads();
    switch (argIn.size()) {
    case 0: {
        retval << ArrayOf::doubleScalar(static_cast<double>(currentValue));
    } break;
    case 1: {
        ArrayOf param1 = argIn[0];
        if (param1.isRowVectorCharacterArray()) {
            std::wstring str = param1.asWideString();
            if (str == L"automatic") {
                setDefaultMaxNumCompThreads();
            } else {
                Error(L"Nelson:invalidValue", L"Wrong value for #%d argument.", (1));
            }
        } else {
            indexType N = param1.asScalarIndex(false);
            setMaxNumCompThreads(static_cast<unsigned int>(N));
        }
        retval << ArrayOf::doubleScalar(static_cast<double>(currentValue));
    } break;
    default: {
        Error(L"Nelson:tooManyInputs", L"Too many input arguments.");
    } break;
    }
    return retval;
}
//=============================================================================
