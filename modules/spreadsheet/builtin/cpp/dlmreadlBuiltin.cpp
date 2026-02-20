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
#include "InputOutputArgumentsCheckers.hpp"
#include "DelimiterFileReader.hpp"
#include "CSVRangeConverter.hpp"
#include "PredefinedErrorMessages.hpp"
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
            raiseError(L"Nelson:spreadsheet:ERROR_DLMREAD_WRONG_VALUE_ARG2_SINGLE_CHAR_EXPECTED",
                ERROR_DLMREAD_WRONG_VALUE_ARG2_SINGLE_CHAR_EXPECTED);
        }
    } break;

    case 3: {
        // filename, delimiter, range ([R1 C1 R2 C2])
        delimiter = argIn[1].getContentAsWideString();
        if (delimiter == L"\\t") {
            delimiter = L"\t";
        }
        if (delimiter.size() > 1) {
            raiseError(L"Nelson:spreadsheet:ERROR_DLMREAD_WRONG_VALUE_ARG2_SINGLE_CHAR_EXPECTED",
                ERROR_DLMREAD_WRONG_VALUE_ARG2_SINGLE_CHAR_EXPECTED);
        }
        if (!argIn[2].isRowVector()) {
            raiseError(L"Nelson:spreadsheet:ERROR_DLMREAD_WRONG_SIZE_FOR_3_ROW_VECTOR_EXPECTED",
                ERROR_DLMREAD_WRONG_SIZE_FOR_3_ROW_VECTOR_EXPECTED);
        }
        ArrayOf param2 = argIn[2];
        if (param2.isScalarStringArray() || param2.isRowVectorCharacterArray()) {
            bool failed = false;
            range = CSVRangeConverter::convertRange(param2.getContentAsCString(), failed);
            if (failed) {
                raiseError(
                    L"Nelson:spreadsheet:ERROR_DLMREAD_WRONG_VALUE_ARG3_VALID_RANGE_EXPECTED",
                    ERROR_DLMREAD_WRONG_VALUE_ARG3_VALID_RANGE_EXPECTED);
            }
        } else {
            if (param2.getElementCount() != 4) {
                raiseError(L"Nelson:spreadsheet:ERROR_DLMREAD_WRONG_SIZE_FOR_3_ROW_VECTOR_EXPECTED",
                    ERROR_DLMREAD_WRONG_SIZE_FOR_3_ROW_VECTOR_EXPECTED);
            }
            if (!param2.isNumeric()) {
                raiseError2(_E("nelson:validators:mustBeNumericAtPosition"), 3);
            }
            if (param2.isSparse()) {
                raiseError2(_E("nelson:validators:mustBeNonSparseAtPosition"), 3);
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
            raiseError(L"Nelson:spreadsheet:ERROR_DLMREAD_WRONG_VALUE_ARG2_SINGLE_CHAR_EXPECTED",
                ERROR_DLMREAD_WRONG_VALUE_ARG2_SINGLE_CHAR_EXPECTED);
        }
        if (!argIn[2].isScalar() || !argIn[2].isIntegerValue()) {
            raiseError2(_E("nelson:validators:mustBeIntegerAtPosition"), 3);
        } else {
            range.push_back(argIn[2].getContentAsDoubleScalar());
        }
        if (!argIn[3].isScalar() || !argIn[3].isIntegerValue()) {
            raiseError2(_E("nelson:validators:mustBeIntegerAtPosition"), 4);
        } else {
            range.push_back(argIn[3].getContentAsDoubleScalar());
        }
    } break;
    default: {
        raiseError2(_E("nelson:arguments:wrongNumberOfInputs"));
    } break;
    }

    std::wstring errorMessage;
    ArrayOf result = delimitedFileReader(filename, delimiter, range, errorMessage);
    if (!errorMessage.empty()) {
        Error(errorMessage, L"Nelson:spreadsheet:ERROR_DLMREAD_ERROR");
    }
    retval << result;

    return retval;
}
//=============================================================================
