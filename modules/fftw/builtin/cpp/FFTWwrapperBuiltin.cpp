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
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::FftwGateway::FFTWwrapperBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 1, 3);
    switch (argIn.size()) {
    case 1: {
        std::wstring param1 = argIn[0].asWideString();
        if (param1 == L"load") {
            retval << ArrayOf::logicalScalar(loadFFTWLibrary());
        } else if (param1 == L"free") {
            retval << ArrayOf::logicalScalar(freeFFTWLibrary());
        } else {
            Error(L"Nelson:fftw:optionValueExpected", L"%s expected.",
                L"Wrong value for #1: 'load' or 'free'");
        }
    } break;
    case 3: {
        std::wstring param1 = argIn[0].asWideString();
        if (param1 == L"load") {
            std::wstring fftwName = argIn[0].asWideString();
            std::wstring fftwfName = argIn[1].asWideString();
            retval << ArrayOf::logicalScalar(loadFFTWLibrary(fftwName, fftwfName));
        } else {
            Error(L"Nelson:fftw:invalidOption", L"Wrong value for #%d: '%s' expected.", 1, L"load");
        }
    } break;
    default: {
        Error(L"Nelson:wrongNumberOfInputs", L"Wrong number of input arguments.");
    } break;
    }
    return retval;
}
//=============================================================================
