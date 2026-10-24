//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#ifdef _MSC_VER
#define _CRT_SECURE_NO_WARNINGS
#endif
//=============================================================================
#include <cstdio>
#include "sscanfBuiltin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "characters_encoding.hpp"
#include "SscanfFunction.hpp"
#include "i18n.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::StreamGateway::sscanfBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 2, 3);
    nargoutcheck(nLhs, 0, 4);
    bool isParam1SupportedType
        = (argIn[0].isStringArray() && argIn[0].isScalar() || argIn[0].isRowVectorCharacterArray());

    if (!isParam1SupportedType) {
        Error("Nelson:validators:mustBeTextScalar",
            "%s must be a character vector or string scalar.", "First argument");
    }
    std::wstring wstr = argIn[0].asWideString();
    ArrayOf param2 = argIn[1];
    std::wstring format = param2.asWideString();

    double m = -1, n = -1;
    bool haveThirdArgument = false;
    if (argIn.size() == 3) {
        ArrayOf param3 = argIn[2];
        param3.promoteType(NLS_DOUBLE);
        Dimensions dims3 = param3.dimensions();
        if (param3.isDoubleType(true)) {
            if (dims3.isScalar() || dims3.elementCount() == 2) {
                if (dims3.isScalar()) {
                    m = param3.asDoubleScalar();
                    if (m < 0) {
                        Error(L"Nelson:stream_manager:invalidSizeValue",
                            L"Wrong value >= 0 expected.");
                    }
                    n = 1;
                } else {
                    double* ptr = (double*)param3.dataPointer();
                    m = ptr[0];
                    if (m < 0) {
                        Error(L"Nelson:stream_manager:invalidSizeValue",
                            L"Wrong value >= 0 expected.");
                    }
                    n = ptr[1];
                    if (n < 0) {
                        Error(L"Nelson:stream_manager:invalidSizeValue",
                            L"Wrong value >= 0 expected.");
                    }
                }
            } else {
                Error(L"Nelson:stream_manager:invalidSize",
                    L"Wrong size. scalar or [a, b] expected.");
            }
        } else {
            Error(L"Nelson:stream_manager:invalidSizeType", L"Wrong type. double expected.");
        }
        haveThirdArgument = true;
    }

    indexType count = 0;
    indexType nextIndex = 0;
    std::wstring errorMessage;

    ArrayOf value
        = SscanF(wstr, format, m, n, haveThirdArgument, count, nextIndex, nLhs > 3, errorMessage);
    retval << value;
    if (nLhs > 1) {
        retval << ArrayOf::doubleScalar((double)count);
    }
    if (nLhs > 2) {
        retval << ArrayOf::characterArray(errorMessage);
    }
    if (nLhs > 3) {
        retval << ArrayOf::doubleScalar((double)nextIndex);
    }
    return retval;
}
//=============================================================================
