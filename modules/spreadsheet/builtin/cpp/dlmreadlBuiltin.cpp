//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "dlmreadBuiltin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "DelimiterFileReader.hpp"
#include "CSVRangeConverter.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::SpreadsheetGateway::dlmreadBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 1, 4);
    std::wstring filename = argIn[0].asWideString();
    std::wstring delimiter;
    std::vector<double> range;

    switch (argIn.size()) {
    case 1: {
    } break;
    case 2: {
        // filename, delimiter
        delimiter = argIn[1].asWideString();
        if (delimiter == L"\\t") {
            delimiter = L"\t";
        }
        if (delimiter.size() > 1) {
            Error(L"Nelson:dlmread:invalidDelimiter",
                L"Wrong value for argument #%d: single character expected.", 2);
        }
    } break;

    case 3: {
        // filename, delimiter, range ([R1 C1 R2 C2])
        delimiter = argIn[1].asWideString();
        if (delimiter == L"\\t") {
            delimiter = L"\t";
        }
        if (delimiter.size() > 1) {
            Error(L"Nelson:dlmread:invalidDelimiter",
                L"Wrong value for argument #%d: single character expected.", 2);
        }
        if (!argIn[2].isRowVector()) {
            Error(L"Nelson:dlmread:invalidRange",
                L"Wrong size for #%d argument. row vector expected.", 3);
        }
        ArrayOf param2 = argIn[2];
        if (param2.isScalarStringArray() || param2.isRowVectorCharacterArray()) {
            bool failed = false;
            range = CSVRangeConverter::convertRange(param2.asUtf8String(), failed);
            if (failed) {
                Error(L"Nelson:dlmread:invalidRange:wrongValueNArgumentValidRange",
                    L"Wrong value for #%d argument. valid range expected.", 3);
            }
        } else {
            if (param2.elementCount() != 4) {
                Error(L"Nelson:dlmread:invalidRange",
                    L"Wrong size for #%d argument. row vector expected.", 3);
            }
            if (!param2.isNumeric()) {
                Error(L"Nelson:dlmread:invalidRange:wrongTypeNArgumentNumericValues",
                    L"Wrong type for #%d argument. numeric values expected.", 3);
            }
            if (param2.isSparse()) {
                Error(L"Nelson:dlmread:invalidRange:wrongTypeNArgumentDenseValues",
                    L"Wrong type for #%d argument. dense values expected.", 3);
            }
            param2.promoteType(NLS_DOUBLE);
            double* values = (double*)param2.dataPointer();
            for (size_t k = 0; k < 4; ++k) {
                range.push_back(values[k]);
            }
        }
    } break;

    case 4: {
        // filename, delimiter, R1, C1
        delimiter = argIn[1].asWideString();
        if (delimiter == L"\\t") {
            delimiter = L"\t";
        }
        if (delimiter.size() > 1) {
            Error(L"Nelson:dlmread:invalidDelimiter",
                L"Wrong value for argument #%d: single character expected.", 2);
        }
        if (!argIn[2].isScalar() || !argIn[2].isIntegerValue()) {
            Error(L"Nelson:dlmread:invalidRow",
                L"Wrong value for argument #%d: integer value expected.", 3);
        } else {
            range.push_back(argIn[2].asDoubleScalar());
        }
        if (!argIn[3].isScalar() || !argIn[3].isIntegerValue()) {
            Error(L"Nelson:dlmread:invalidColumn",
                L"Wrong value for argument #%d: integer value expected.", 4);
        } else {
            range.push_back(argIn[3].asDoubleScalar());
        }
    } break;
    default:
        break;
    }

    std::wstring errorMessage;
    ArrayOf result = delimitedFileReader(filename, delimiter, range, errorMessage);
    if (!errorMessage.empty()) {
        Error(L"Nelson:dlmread:readFailed", errorMessage);
    }
    retval << result;

    return retval;
}
//=============================================================================
