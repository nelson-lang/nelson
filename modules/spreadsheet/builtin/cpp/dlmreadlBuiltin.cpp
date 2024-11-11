//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "dlmreadBuiltin.hpp"
#include "Error.hpp"
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
    std::wstring filename = argIn[0].getContentAsWideString();
    std::wstring delimiter;
    std::vector<double> range;

    switch (argIn.size()) {
    case 1: {
        // nothing to do
    } break;
    case 2: {
        // filename, delimiter
        delimiter = argIn[1].getContentAsWideString();
        if (delimiter == L"\\t") {
            delimiter = L"\t";
        }
        if (delimiter.size() > 1) {
            Error(_W("Wrong value for argument #2: single character expected."));
        }
    } break;

    case 3: {
        // filename, delimiter, range ([R1 C1 R2 C2])
        delimiter = argIn[1].getContentAsWideString();
        if (delimiter == L"\\t") {
            delimiter = L"\t";
        }
        if (delimiter.size() > 1) {
            Error(_W("Wrong value for argument #2: single character expected."));
        }
        if (!argIn[2].isRowVector()) {
            Error(_W("Wrong size for #3 argument. row vector expected."));
        }
        ArrayOf param2 = argIn[2];
        if (param2.isScalarStringArray() || param2.isRowVectorCharacterArray()) {
            bool failed = false;
            range = CSVRangeConverter::convertRange(param2.getContentAsCString(), failed);
            if (failed) {
                Error(_W("Wrong value for #3 argument. valid range expected."));
            }
        } else {
            if (param2.getElementCount() != 4) {
                Error(_W("Wrong size for #3 argument. row vector expected."));
            }
            if (!param2.isNumeric()) {
                Error(_W("Wrong type for #3 argument. numeric values expected."));
            }
            if (param2.isSparse()) {
                Error(_W("Wrong type for #3 argument. dense values expected."));
            }
            param2.promoteType(NLS_DOUBLE);
            double* values = (double*)param2.getDataPointer();
            for (size_t k = 0; k < 4; ++k) {
                range.push_back(values[k]);
            }
        }
    } break;

    case 4: {
        // filename, delimiter, R1, C1
        delimiter = argIn[1].getContentAsWideString();
        if (delimiter == L"\\t") {
            delimiter = L"\t";
        }
        if (delimiter.size() > 1) {
            Error(_W("Wrong value for argument #2: single character expected."));
        }
        if (!argIn[2].isScalar() || !argIn[2].isIntegerValue()) {
            Error(_W("Wrong value for argument #3: integer value expected."));
        } else {
            range.push_back(argIn[2].getContentAsDoubleScalar());
        }
        if (!argIn[3].isScalar() || !argIn[3].isIntegerValue()) {
            Error(_W("Wrong value for argument #4: integer value expected."));
        } else {
            range.push_back(argIn[3].getContentAsDoubleScalar());
        }
    } break;
    default: {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    } break;
    }

    std::wstring errorMessage;
    ArrayOf result = delimitedFileReader(filename, delimiter, range, errorMessage);
    if (!errorMessage.empty()) {
        Error(errorMessage);
    }
    retval << result;

    return retval;
}
//=============================================================================
