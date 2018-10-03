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
#include "schurBuiltin.hpp"
#include "Error.hpp"
#include "OverloadFunction.hpp"
#include "OverloadRequired.hpp"
#include "SchurDecompostion.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::LinearAlgebraGateway::schurBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (argIn.size() > 2 || argIn.size() < 1) {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    if (nLhs > 2) {
        Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
    // Call overload if it exists
    bool bSuccess = false;
    if (eval->mustOverloadBasicTypes()) {
        retval = OverloadFunction(eval, nLhs, argIn, "schur", bSuccess);
    }
    if (!bSuccess) {
        if (argIn[0].isReferenceType() || argIn[0].isSparse() || argIn[0].isLogical()
            || argIn[0].isCharacterArray() || argIn[0].isIntegerType()) {
            retval = OverloadFunction(eval, nLhs, argIn, "schur", bSuccess);
            if (bSuccess) {
                return retval;
            }
            OverloadRequired(eval, argIn, Overload::OverloadClass::FUNCTION);
        }
        bool asComplex = false;
        if (argIn.size() == 2) {
            ArrayOf param2 = argIn[1];
            std::wstring str = param2.getContentAsWideString();
            if (str == L"complex" || str == L"real") {
                if (str == L"complex") {
                    asComplex = true;
                }
            } else {
                Error(_W("Second input argument must be 'real' or 'complex'."));
            }
        }
        if (nLhs == 2) {
            ArrayOf U;
            ArrayOf T;
            SchurDecomposition(argIn[0], asComplex, U, T);
            retval.push_back(U);
            retval.push_back(T);
        } else {
            ArrayOf T;
            SchurDecomposition(argIn[0], asComplex, T);
            retval.push_back(T);
        }
    }
    return retval;
}
//=============================================================================
