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
#include "issymmetricBuiltin.hpp"
#include "Error.hpp"
#include "OverloadFunction.hpp"
#include "OverloadRequired.hpp"
#include "IsSymmetric.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::LinearAlgebraGateway::issymmetricBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (!(argIn.size() == 1 || argIn.size() == 2)) {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    if (nLhs > 1) {
        Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
    bool skew = false;
    double tol = 0;
    bool withTol = false;
    if (argIn.size() == 2) {
        ArrayOf param2 = argIn[1];
        if (param2.isRowVectorCharacterArray()) {
            withTol = false;
            std::wstring str = param2.getContentAsWideString();
            if (str == L"skew" || str == L"nonskew") {
                if (str == L"skew") {
                    skew = true;
                } else {
                    skew = false;
                }
            } else {
                Error(_W("Second input must be 'skew' or 'nonskew'."));
            }
        } else {
            withTol = true;
            tol = param2.getContentAsDoubleScalar();
            if (!std::isfinite(tol) || tol < 0.) {
                Error(_W("Second input must be finite and >= 0."));
            }
        }
    }
    // Call overload if it exists
    bool bSuccess = false;
    if (eval->mustOverloadBasicTypes()) {
        retval = OverloadFunction(eval, nLhs, argIn, "issymmetric", bSuccess);
    }
    if (!bSuccess) {
        if (argIn[0].isReferenceType() || argIn[0].isSparse() || argIn[0].isLogical()
            || argIn[0].isCharacterArray()) {
            retval = OverloadFunction(eval, nLhs, argIn, "issymmetric", bSuccess);
            if (bSuccess) {
                return retval;
            }
            OverloadRequired(eval, argIn, Overload::OverloadClass::FUNCTION);
        }
        if (withTol) {
            retval.push_back(ArrayOf::logicalConstructor(IsSymmetric(argIn[0], tol)));
        } else {
            retval.push_back(ArrayOf::logicalConstructor(IsSymmetric(argIn[0], skew)));
        }
    }
    return retval;
}
//=============================================================================
