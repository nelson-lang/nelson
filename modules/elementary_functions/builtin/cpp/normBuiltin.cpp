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
#include "normBuiltin.hpp"
#include "Error.hpp"
#include "OverloadFunction.hpp"
#include "Norm.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::ElementaryFunctionsGateway::normBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (argIn.size() < 1 || argIn.size() > 2) {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    if (nLhs > 1) {
        Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
    bool bSuccess = false;
    if (eval->mustOverloadBasicTypes()) {
        retval = OverloadFunction(eval, nLhs, argIn, "norm", bSuccess);
    }
    if (!bSuccess) {
        if (argIn[0].getDataClass() == NLS_SINGLE || argIn[0].getDataClass() == NLS_SCOMPLEX
            || argIn[0].getDataClass() == NLS_DOUBLE || argIn[0].getDataClass() == NLS_DCOMPLEX) {
            if (argIn[0].isSparse()) {
                retval = OverloadFunction(eval, nLhs, argIn, "norm", bSuccess);
                if (!bSuccess) {
                    Error(_W("Sparse not supported."));
                }
            } else {
                if (argIn.size() > 1) {
                    if (argIn[1].isRowVectorCharacterArray()) {
                        std::wstring param = argIn[1].getContentAsWideString();
                        if (param == L"fro") {
                            retval.push_back(NormFrobenius(argIn[0]));
                        } else {
                            Error(ERROR_WRONG_ARGUMENT_2_VALUE);
                        }
                    } else {
                        ArrayOf param = argIn[1];
                        double p = param.getContentAsDoubleScalar();
                        retval.push_back(Norm(argIn[0], p));
                    }
                } else {
                    retval.push_back(Norm(argIn[0], 2));
                }
            }
        } else {
            retval = OverloadFunction(eval, nLhs, argIn, "norm", bSuccess);
            if (!bSuccess) {
                Error(ERROR_WRONG_ARGUMENT_1_TYPE);
            }
        }
    }
    return retval;
}
//=============================================================================
