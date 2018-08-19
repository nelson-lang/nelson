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
#include <Eigen/Dense>
#include <boost/algorithm/string.hpp>
#include "int2strBuiltin.hpp"
#include "Error.hpp"
#include "IntegerToString.hpp"
#include "OverloadFunction.hpp"
#include "VertCat.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
static ArrayOf
StringVectorToString(wstringVector V, Dimensions DimsV)
{
    ArrayOf strArr;
    if (V.size() == 0) {
        strArr = ArrayOf::emptyConstructor(Dimensions(1, 0));
        strArr.promoteType(NLS_CHAR);
    } else {
        if (V.size() == 1) {
            strArr = ArrayOf::stringConstructor(V[0]);
        } else {
            wstringVector L;
            size_t lenMax = 0;
            for (size_t k = 0; k < V.size(); k++) {
                lenMax = std::max(lenMax, V[k].size());
            }
            std::wstring line;
            size_t q = 0;
            size_t R = DimsV[0];
            size_t C = DimsV.getElementCount() / DimsV[0];
            for (size_t r = 0; r < R; r++) {
                for (size_t c = 0; c < C; c++) {
                    size_t m = r + c * R;
                    std::wstring spaces = L"  ";
                    if (lenMax > V[m].size()) {
                        for (size_t l = 0; l < lenMax - V[m].size(); l++) {
                            spaces.push_back(L' ');
                        }
                    }
                    line = line + spaces + V[m];
                    if (q == C - 1) {
                        if (r == 0) {
                            strArr = ArrayOf::stringConstructor(line);
                        } else {
                            bool bSuccess;
                            ArrayOf B = ArrayOf::stringConstructor(line);
                            strArr = VertCat(strArr, B, true, bSuccess);
                        }
                        line = L"";
                        q = 0;
                    } else {
                        q++;
                    }
                }
            }
        }
    }
    return strArr;
}
//=============================================================================
ArrayOfVector
Nelson::StringGateway::int2strBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (nLhs > 1) {
        Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
    if (argIn.size() != 1) {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    // Call overload if it exists
    bool bSuccess = false;
    if (eval->overloadOnBasicTypes) {
        retval = OverloadFunction(eval, nLhs, argIn, "int2str", bSuccess);
    }
    if (!bSuccess) {
        wstringVector result;
        std::wstring error_message;
        bool bRes = IntegerToString(argIn[0], result, error_message);
        if (bRes) {
            retval.push_back(StringVectorToString(result, argIn[0].getDimensions()));
        } else {
            retval = OverloadFunction(eval, nLhs, argIn, "int2str", bSuccess);
            if (!bSuccess) {
                Error(error_message);
            }
        }
    }
    return retval;
}
//=============================================================================
