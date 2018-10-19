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
#include "lapack_eigen.hpp"
#include <unsupported/Eigen/MatrixFunctions>
#include "MatrixPower.hpp"
#include "DotPower.hpp"
#include "InverseMatrix.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
template <class T>
ArrayOf
complexMatrixScalarPower(ArrayOf& A, ArrayOf& B)
{
    Error(_W("Power (^) currently not implemented in Nelson."));
    return ArrayOf();
}
//=============================================================================
template <class T>
ArrayOf
realMatrixScalarPower(ArrayOf& A, ArrayOf& B)
{
    Error(_W("Power (^) currently not implemented in Nelson."));
    return ArrayOf();
}
//=============================================================================
static ArrayOf
matrixScalarPower(ArrayOf& A, ArrayOf& B, bool& needToOverload)
{
    if (A.isSingleClass()) {
        if (A.isComplex()) {
            return complexMatrixScalarPower<single>(A, B);
        } else {
            return realMatrixScalarPower<single>(A, B);
        }
    } else {
        if (A.isComplex()) {
            return complexMatrixScalarPower<double>(A, B);
        } else {
            return realMatrixScalarPower<double>(A, B);
        }
    }
    return ArrayOf();
}
//=============================================================================
static ArrayOf
scalarMatrixPower(ArrayOf& A, ArrayOf& B, bool& needToOverload)
{
    Error(_W("Power (^) currently not implemented in Nelson."));
    return ArrayOf();
}
//=============================================================================
ArrayOf
MatrixPower(ArrayOf& A, ArrayOf& B, bool& needToOverload)
{
    needToOverload = false;
    if (A.isScalar() && B.isScalar()) {
        return DotPower(A, B, needToOverload);
    }
    // Check for A & B numeric
    bool isReference = A.isReferenceType() || B.isReferenceType();
    if (isReference) {
        needToOverload = true;
        return ArrayOf();
    }

    if (A.isEmpty() || B.isEmpty()) {
        Dimensions dims;
        if (A.isEmpty()) {
            dims = A.getDimensions();
        } else {
            dims = B.getDimensions();
        }
        return ArrayOf::emptyConstructor(dims);
    }

    // Test for 2D on both A & B
    if (!A.is2D() || !B.is2D()) {
        Error(_W("Cannot apply exponential operation to N-Dimensional arrays."));
    }

    if (B.isReal() && B.isScalar()) {
        ArrayOf C(B);
        C.promoteType(NLS_DOUBLE);
        double value = C.getContentAsDoubleScalar();
        if (value == rint(value) && (value == -1)) {
            return InverseMatrix(A, needToOverload);
        }
    }
    // Both arguments must be square
    if ((A.getDimensionLength(0) != A.getDimensionLength(1))
        || (B.getDimensionLength(0) != B.getDimensionLength(1))) {
        Error(_W("Power (^) operator can only be applied to scalar and square arguments."));
    }

    bool isSupportedType = (A.isSingleClass() || A.isDoubleClass())
        && (B.isSingleClass() || B.isDoubleClass()) && !A.isSparse() && !B.isSparse();
    if (!isSupportedType) {
        needToOverload = true;
        return ArrayOf();
    }

    if (A.getDataClass() != B.getDataClass()) {
        if (A.getDataClass() > B.getDataClass()) {
            B.promoteType(A.getDataClass());
        } else {
            A.promoteType(B.getDataClass());
        }
    }

    ArrayOf res;
    if (B.isScalar()) {
        res = matrixScalarPower(A, B, needToOverload);
    } else {
        res = scalarMatrixPower(A, B, needToOverload);
    }
    return res;
}
//=============================================================================
}
//=============================================================================
