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
#include "HorzCatInteger.hpp"
#include "ConcatenateNdArray.hpp"
#include "Error.hpp"
#include <Eigen/Dense>
//=============================================================================
namespace Nelson {
//=============================================================================
template <class T>
ArrayOf
HorzCatIntTemplate(ArrayOf A, ArrayOf B, Dimensions dimsRes)
{
    T* ptrA = (T*)A.getDataPointer();
    T* ptrB = (T*)B.getDataPointer();
    void* pRes
        = ArrayOf::allocateArrayOf(A.getDataClass(), dimsRes.getRows() * dimsRes.getColumns());
    T* ptrC = (T*)pRes;
    Dimensions dimsA = A.getDimensions();
    Dimensions dimsB = B.getDimensions();
    Eigen::Map<Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic>> matA(
        ptrA, dimsA.getRows(), dimsA.getColumns());
    Eigen::Map<Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic>> matB(
        ptrB, dimsB.getRows(), dimsB.getColumns());
    Eigen::Map<Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic>> matC(
        ptrC, dimsRes.getRows(), dimsRes.getColumns());
    matC << matA, matB;
    return ArrayOf(A.getDataClass(), dimsRes, pRes);
}
//=============================================================================
ArrayOf
HorzCatInteger(ArrayOf A, ArrayOf B)
{
    if (!A.isIntegerType()) {
        Error(ERROR_WRONG_ARGUMENT_1_TYPE_INTEGER_EXPECTED);
    }
    if (!B.isIntegerType()) {
        Error(ERROR_WRONG_ARGUMENT_2_TYPE_INTEGER_EXPECTED);
    }
    if (!A.is2D()) {
        Error(ERROR_WRONG_ARGUMENT_1_SIZE_INTEGER_EXPECTED);
    }
    if (!B.is2D()) {
        Error(ERROR_WRONG_ARGUMENT_2_SIZE_INTEGER_EXPECTED);
    }
    Class classA = A.getDataClass();
    Class classB = B.getDataClass();
    if (classA != classB) {
        Error(ERROR_SAME_INTEGER_TYPE_EXPECTED);
    }
    if (A.isEmpty(false)) {
        ArrayOf C(B);
        return C;
    }
    if (B.isEmpty(false)) {
        ArrayOf C(A);
        return C;
    }
    Class classC = classA;
    Dimensions dimsA = A.getDimensions();
    Dimensions dimsB = B.getDimensions();
    if (dimsA.getRows() != dimsB.getRows()) {
        Error(ERROR_DIMENSIONS_NOT_CONSISTENT);
    }
    indexType newColumnsSize = dimsA.getColumns() + dimsB.getColumns();
    indexType newRowsSize = dimsA.getRows();
    Dimensions dimsC = Dimensions(newRowsSize, newColumnsSize);
    ArrayOf C;
    switch (classC) {
    case NLS_INT8:
        C = HorzCatIntTemplate<int8>(A, B, dimsC);
        break;
    case NLS_INT16:
        C = HorzCatIntTemplate<int16>(A, B, dimsC);
        break;
    case NLS_INT32:
        C = HorzCatIntTemplate<int32>(A, B, dimsC);
        break;
    case NLS_INT64:
        C = HorzCatIntTemplate<int64>(A, B, dimsC);
        break;
    case NLS_UINT8:
        C = HorzCatIntTemplate<uint8>(A, B, dimsC);
        break;
    case NLS_UINT16:
        C = HorzCatIntTemplate<uint16>(A, B, dimsC);
        break;
    case NLS_UINT32:
        C = HorzCatIntTemplate<uint32>(A, B, dimsC);
        break;
    case NLS_UINT64:
        C = HorzCatIntTemplate<uint64>(A, B, dimsC);
        break;
    default:
        break;
    }
    return C;
}
//=============================================================================
ArrayOf
HorzCatNdArrayInteger(ArrayOf A, ArrayOf B)
{
    if (!A.isNdArrayIntegerType()) {
        Error(ERROR_WRONG_ARGUMENT_1_TYPE_INTEGER_EXPECTED);
    }
    if (!B.isNdArrayIntegerType()) {
        Error(ERROR_WRONG_ARGUMENT_2_TYPE_INTEGER_EXPECTED);
    }
    Dimensions dimsA = A.getDimensions();
    Dimensions dimsB = B.getDimensions();
    if (dimsA.getRows() != dimsB.getRows()) {
        Error(ERROR_DIMENSIONS_NOT_CONSISTENT);
    }
    if (dimsA.getLength() != dimsB.getLength()) {
        Error(ERROR_DIMENSIONS_NOT_CONSISTENT);
    }
    for (indexType k = 0; k < dimsA.getLength(); k++) {
        if (k != 1) {
            if (dimsA.getDimensionLength(k) != dimsB.getDimensionLength(k)) {
                Error(ERROR_DIMENSIONS_NOT_CONSISTENT);
            }
        }
    }
    Class classA = A.getDataClass();
    Class classB = B.getDataClass();
    Class classC;
    if (classA == classB) {
        classC = classA;
    } else {
        Error(ERROR_SAME_INTEGER_TYPE_EXPECTED);
    }
    ArrayOfMatrix m;
    ArrayOfVector v;
    v.push_back(A);
    v.push_back(B);
    m.push_back(v);
    return ConcatenateNdArray(m, classC);
}
//=============================================================================
}
//=============================================================================
