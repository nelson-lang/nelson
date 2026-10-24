//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <limits>
#include "audioreadBuiltin.hpp"
#include "AudioRead.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::AudioGateway::audioreadBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 2);
    nargincheck(argIn, 1, 3);
    std::wstring errorMessage;
    std::wstring datatype = L"double";
    double start = 1;
    double end = std::numeric_limits<double>::infinity();
    std::wstring filename;
    switch (argIn.size()) {
    case 1: {
        ArrayOf param1 = argIn[0];
        filename = param1.asWideString();
    } break;
    case 2: {
        ArrayOf param1 = argIn[0];
        filename = param1.asWideString();
        ArrayOf param2 = argIn[1];
        if (param2.isCharacterArray()) {
            datatype = param2.asWideString();
        } else {
            if (param2.isVector() && param2.isNumeric() && (param2.elementCount() == 2)) {
                param2.promoteType(NLS_DOUBLE);
                auto* ptr = (double*)param2.dataPointer();
                start = ptr[0];
                end = ptr[1];
                if (start < 1 || end < 1) {
                    Error(L"Nelson:audio:invalidIndex", L"Index >= 1 expected.");
                }
            } else {
                Error(L"Nelson:audioread:invalidRangeVector", L"[start, end] vector expected.");
            }
        }
    } break;
    case 3: {
        ArrayOf param1 = argIn[0];
        filename = param1.asWideString();
        ArrayOf param2 = argIn[1];
        if (param2.isVector() && param2.isNumeric() && (param2.elementCount() == 2)) {
            param2.promoteType(NLS_DOUBLE);
            auto* ptr = (double*)param2.dataPointer();
            start = ptr[0];
            end = ptr[1];
            if (start < 1 || end < 1) {
                Error(L"Nelson:audio:invalidIndex", L"Index >= 1 expected.");
            }
            start = start - 1;
            end = end - 1;
        } else {
            Error(L"Nelson:audioread:invalidRangeVector", L"[start, end] vector expected.");
        }
        ArrayOf param3 = argIn[2];
        datatype = param3.asWideString();
    } break;
    }
    retval = AudioRead(filename, start, end, datatype, errorMessage);
    if (!errorMessage.empty()) {
        Error(L"Nelson:audioread:readFailed", errorMessage);
    }
    return retval;
}
//=============================================================================
