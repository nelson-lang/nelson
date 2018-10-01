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
#include "datenumBuiltin.hpp"
#include "DateNumber.hpp"
#include "Error.hpp"
#include "Now.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::TimeGateway::datenumBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (nLhs > 1) {
        Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
    double year = 0.;
    double month = 0.;
    double day = 0.;
    double hour = 0.;
    double min = 0.;
    double sec = 0.;
    double res = 0.;
    switch (argIn.size()) {
    case 0: {
        res = Now();
    } break;
    case 1: {
        ArrayOf param1 = argIn[0];
        if (param1.isNumeric()) {
            if (param1.isScalar()) {
                year = param1.getContentAsDoubleScalar();
            } else if (param1.isEmpty()) {
                Dimensions dims = param1.getDimensions();
                retval.push_back(ArrayOf::emptyConstructor(dims));
                return retval;
            } else if (param1.isRowVector()) {
                indexType len = param1.getLength();
                if (len == 3) {
                    param1.promoteType(NLS_DOUBLE);
                    double* ptrDouble = (double*)param1.getDataPointer();
                    year = ptrDouble[0];
                    month = ptrDouble[1];
                    day = ptrDouble[2];
                    res = DateNumber(year, month, day, hour, min, sec);
                } else if (len == 6) {
                    param1.promoteType(NLS_DOUBLE);
                    double* ptrDouble = (double*)param1.getDataPointer();
                    year = ptrDouble[0];
                    month = ptrDouble[1];
                    day = ptrDouble[2];
                    hour = ptrDouble[3];
                    min = ptrDouble[4];
                    sec = ptrDouble[5];
                    res = DateNumber(year, month, day, hour, min, sec);
                } else {
                    retval.push_back(param1);
                    return retval;
                }
            } else if (param1.isColumnVector()) {
                retval.push_back(param1);
                return retval;
            } else if (param1.is2D()) {
                if (param1.getDimensions().getColumns() == 3) {
                    // OK
                } else if (param1.getDimensions().getColumns() == 6) {
                    // OK
                } else {
                    retval.push_back(param1);
                    return retval;
                }
            } else {
                retval.push_back(param1);
                return retval;
            }
        } else {
            if (param1.isRowVectorCharacterArray()) {
                std::wstring strdate = param1.getContentAsWideString();
                bool bParsed;
                res = DateNumber(strdate, bParsed);
                if (!bParsed) {
                    Error(L"None of the standard formats match the DATE string.");
                }
            } else {
                Error(ERROR_WRONG_ARGUMENT_1_TYPE_DOUBLE_EXPECTED);
            }
        }
    } break;
    case 2: {
        ArrayOf param1 = argIn[0];
        ArrayOf param2 = argIn[1];
        if (param1.isRowVectorCharacterArray() && param2.isRowVectorCharacterArray()) {
            std::wstring datestr = param1.getContentAsWideString();
            std::wstring dateformat = param2.getContentAsWideString();
            bool bParsed = false;
            res = DateNumber(datestr, dateformat, bParsed);
            if (!bParsed) {
                Error(L"format does not match the DATE string.");
            }
        } else {
            Error(ERROR_WRONG_ARGUMENTS_TYPE);
        }
    } break;
    case 3: {
        ArrayOf param1 = argIn[0];
        if (!param1.isNumeric()) {
            Error(ERROR_WRONG_ARGUMENT_1_TYPE_DOUBLE_EXPECTED);
        }
        year = param1.getContentAsDoubleScalar();
        ArrayOf param2 = argIn[1];
        if (!param2.isNumeric()) {
            Error(ERROR_WRONG_ARGUMENT_2_TYPE_DOUBLE_EXPECTED);
        }
        month = param2.getContentAsDoubleScalar();
        ArrayOf param3 = argIn[2];
        if (!param3.isNumeric()) {
            Error(ERROR_WRONG_ARGUMENT_3_TYPE_DOUBLE_EXPECTED);
        }
        day = param3.getContentAsDoubleScalar();
        res = DateNumber(year, month, day, hour, min, sec);
    } break;
    case 6: {
        ArrayOf param1 = argIn[0];
        if (!param1.isNumeric()) {
            Error(ERROR_WRONG_ARGUMENT_1_TYPE_DOUBLE_EXPECTED);
        }
        year = param1.getContentAsDoubleScalar();
        ArrayOf param2 = argIn[1];
        if (!param2.isNumeric()) {
            Error(ERROR_WRONG_ARGUMENT_2_TYPE_DOUBLE_EXPECTED);
        }
        month = param2.getContentAsDoubleScalar();
        ArrayOf param3 = argIn[2];
        if (!param3.isNumeric()) {
            Error(ERROR_WRONG_ARGUMENT_3_TYPE_DOUBLE_EXPECTED);
        }
        day = param3.getContentAsDoubleScalar();
        ArrayOf param4 = argIn[3];
        if (!param4.isNumeric()) {
            Error(ERROR_WRONG_ARGUMENT_4_TYPE_DOUBLE_EXPECTED);
        }
        hour = param4.getContentAsDoubleScalar();
        ArrayOf param5 = argIn[4];
        if (!param5.isNumeric()) {
            Error(ERROR_WRONG_ARGUMENT_5_TYPE_DOUBLE_EXPECTED);
        }
        min = param5.getContentAsDoubleScalar();
        ArrayOf param6 = argIn[5];
        if (!param6.isNumeric()) {
            Error(ERROR_WRONG_ARGUMENT_6_TYPE_DOUBLE_EXPECTED);
        }
        sec = param6.getContentAsDoubleScalar();
        res = DateNumber(year, month, day, hour, min, sec);
    } break;
    default: {
        Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    } break;
    }
    retval.push_back(ArrayOf::doubleConstructor(res));
    return retval;
}
//=============================================================================
