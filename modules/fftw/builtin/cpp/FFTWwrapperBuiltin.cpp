//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "FFTWwrapperBuiltin.hpp"
#include "FFTWDynamicLibrary.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "PredefinedErrorMessages.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::FftwGateway::FFTWwrapperBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);
    switch (argIn.size()) {
    case 1: {
        std::wstring param1 = argIn[0].getContentAsWideString();
        if (param1 == L"load") {
            retval << ArrayOf::logicalConstructor(loadFFTWLibrary());
        } else if (param1 == L"free") {
            retval << ArrayOf::logicalConstructor(freeFFTWLibrary());
        } else {
            Error(_W("Wrong value for #1: 'load' or 'free' expected."));
        }
    } break;
    case 3: {
        std::wstring param1 = argIn[0].getContentAsWideString();
        if (param1 == L"load") {
            std::wstring fftwName = argIn[0].getContentAsWideString();
            std::wstring fftwfName = argIn[1].getContentAsWideString();
            retval << ArrayOf::logicalConstructor(loadFFTWLibrary(fftwName, fftwfName));
        } else {
            Error(_W("Wrong value for #1: 'load' expected."));
        }
    } break;
    default: {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    } break;
    }
    return retval;
}
//=============================================================================
