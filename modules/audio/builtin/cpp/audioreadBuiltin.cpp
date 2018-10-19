//=============================================================================
// Copyright (c) 2016-2018 Allan CORNET (Nelson)
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#include "audioreadBuiltin.hpp"
#include "AudioRead.hpp"
#include "Error.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::AudioGateway::audioreadBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (nLhs > 2) {
        Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
    if (argIn.size() == 0 || argIn.size() > 3) {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    std::wstring errorMessage;
    std::wstring datatype = L"double";
    double start = 1;
    double end = std::numeric_limits<double>::infinity();
    std::wstring filename;
    switch (argIn.size()) {
    case 1: {
        ArrayOf param1 = argIn[0];
        filename = param1.getContentAsWideString();
    } break;
    case 2: {
        ArrayOf param1 = argIn[0];
        filename = param1.getContentAsWideString();
        ArrayOf param2 = argIn[1];
        if (param2.isCharacterArray()) {
            datatype = param2.getContentAsWideString();
        } else {
            Dimensions dimsParam2 = param2.getDimensions();
            if (param2.isVector() && param2.isNumeric() && (dimsParam2.getElementCount() == 2)) {
                param2.promoteType(NLS_DOUBLE);
                double* ptr = (double*)param2.getDataPointer();
                start = ptr[0];
                end = ptr[1];
                if (start < 1 || end < 1) {
                    Error(_W("Index >= 1 expected."));
                }
            } else {
                Error(_W("[start, end] vector expected."));
            }
        }
    } break;
    case 3: {
        ArrayOf param1 = argIn[0];
        filename = param1.getContentAsWideString();
        ArrayOf param2 = argIn[1];
        Dimensions dimsParam2 = param2.getDimensions();
        if (param2.isVector() && param2.isNumeric() && (dimsParam2.getElementCount() == 2)) {
            param2.promoteType(NLS_DOUBLE);
            double* ptr = (double*)param2.getDataPointer();
            start = ptr[0];
            end = ptr[1];
            if (start < 1 || end < 1) {
                Error(_W("Index >= 1 expected."));
            }
            start = start - 1;
            end = end - 1;
        } else {
            Error(_W("[start, end] vector expected."));
        }
        ArrayOf param3 = argIn[2];
        datatype = param3.getContentAsWideString();
    } break;
    default: {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    } break;
    }
    retval = AudioRead(filename, start, end, datatype, errorMessage);
    if (errorMessage != L"") {
        Error(errorMessage);
    }
    return retval;
}
//=============================================================================
