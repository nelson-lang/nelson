//=============================================================================
// Copyright (c) 2016-2017 Allan CORNET (Nelson)
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
#include "double_ne_doubleBuiltin.hpp"
#include "NeqDouble.hpp"
#include "Error.hpp"
#include "MatrixCheck.hpp"
#include "OverloadBinaryOperator.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector Nelson::DoubleGateway::double_ne_doubleBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    if (argIn.size() != 2)
    {
        Error(eval, ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    ArrayOf A = argIn[0];
    ArrayOf B = argIn[1];
    if (!(SameSizeCheck(A.getDimensions(), B.getDimensions()) || A.isScalar() || B.isScalar()))
    {
        Error(eval, _W("Size mismatch on arguments to arithmetic operator ") + L"~=");
    }
    if (!A.isDoubleType() || !B.isDoubleType())
    {
        Error(eval, ERROR_WRONG_ARGUMENTS_TYPE_DOUBLE_EXPECTED);
    }
    if (A.isSparse() || B.isSparse())
    {
        Error(eval, ERROR_WRONG_ARGUMENTS_SIZE_FULL_MATRIX_EXPECTED);
    }
    if (!A.is2D() || !B.is2D())
    {
        Error(eval, ERROR_WRONG_ARGUMENTS_SIZE_2D_MATRIX_EXPECTED);
    }
    ArrayOfVector retval;
    ArrayOf res;
    if (A.isComplex() || B.isComplex())
    {
        bool bSuccess = false;
        res = OverloadBinaryOperator(eval, A, B, "ne", bSuccess, "dcomplex_ne_dcomplex");
        if (!bSuccess)
        {
            Error(eval, _W("complex comparaison not defined."));
        }
    }
    else
    {
        res = double_ne_double(A, B);
    }
    retval.push_back(res);
    return retval;
}
//=============================================================================
