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
#include "VertCatInteger.hpp"
#include "ConcatenateNdArray.hpp"
#include "Exception.hpp"
#include <Eigen/Dense>
//=============================================================================
namespace Nelson {
//=============================================================================
template <class T>
ArrayOf
VertCatIntTemplate(ArrayOf A, ArrayOf B, Dimensions dimsRes)
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
VertCatInteger(ArrayOf A, ArrayOf B)
{
    if (!A.isIntegerType()) {
        throw Exception(ERROR_WRONG_ARGUMENT_1_TYPE_INTEGER_EXPECTED);
    }
    if (!B.isIntegerType()) {
        throw Exception(ERROR_WRONG_ARGUMENT_2_TYPE_INTEGER_EXPECTED);
    }
    if (!A.is2D()) {
        throw Exception(ERROR_WRONG_ARGUMENT_1_SIZE_INTEGER_EXPECTED);
    }
    if (!B.is2D()) {
        throw Exception(ERROR_WRONG_ARGUMENT_2_SIZE_INTEGER_EXPECTED);
    }
    Class classA = A.getDataClass();
    Class classB = B.getDataClass();
    if (classA != classB) {
        throw Exception(ERROR_SAME_INTEGER_TYPE_EXPECTED);
    }
    if (A.isEmpty(false)) {
        ArrayOf C(B);
        return C;
    }
    if (B.isEmpty(false)) {
        ArrayOf C(A);
        return C;
    }
    Dimensions dimsA = A.getDimensions();
    Dimensions dimsB = B.getDimensions();
    if (dimsA.getColumns() != dimsB.getColumns()) {
        throw Exception(ERROR_DIMENSIONS_NOT_CONSISTENT);
    }
    Class classC = classA;
    indexType newColumnsSize = dimsA.getColumns();
    indexType newRowsSize = dimsA.getRows() + dimsB.getRows();
    Dimensions dimsC = Dimensions(newRowsSize, newColumnsSize);
    ArrayOf C;
    switch (classC) {
    case NLS_INT8:
        C = VertCatIntTemplate<int8>(A, B, dimsC);
        break;
    case NLS_INT16:
        C = VertCatIntTemplate<int16>(A, B, dimsC);
        break;
    case NLS_INT32:
        C = VertCatIntTemplate<int32>(A, B, dimsC);
        break;
    case NLS_INT64:
        C = VertCatIntTemplate<int64>(A, B, dimsC);
        break;
    case NLS_UINT8:
        C = VertCatIntTemplate<uint8>(A, B, dimsC);
        break;
    case NLS_UINT16:
        C = VertCatIntTemplate<uint16>(A, B, dimsC);
        break;
    case NLS_UINT32:
        C = VertCatIntTemplate<uint32>(A, B, dimsC);
        break;
    case NLS_UINT64:
        C = VertCatIntTemplate<uint64>(A, B, dimsC);
        break;
    default:
        break;
    }
    return C;
}
//=============================================================================
ArrayOf
VertCatNdArrayInteger(ArrayOf A, ArrayOf B)
{
    if (!A.isNdArrayIntegerType()) {
        throw Exception(ERROR_WRONG_ARGUMENT_1_TYPE_INTEGER_EXPECTED);
    }
    if (!B.isNdArrayIntegerType()) {
        throw Exception(ERROR_WRONG_ARGUMENT_2_TYPE_INTEGER_EXPECTED);
    }
    Dimensions dimsA = A.getDimensions();
    Dimensions dimsB = B.getDimensions();
    if (dimsA.getColumns() != dimsB.getColumns()) {
        throw Exception(ERROR_DIMENSIONS_NOT_CONSISTENT);
    }
    if (dimsA.getLength() != dimsB.getLength()) {
        throw Exception(ERROR_DIMENSIONS_NOT_CONSISTENT);
    }
    for (indexType k = 0; k < dimsA.getLength(); k++) {
        if (k != 0) {
            if (dimsA.getDimensionLength(k) != dimsB.getDimensionLength(k)) {
                throw Exception(ERROR_DIMENSIONS_NOT_CONSISTENT);
            }
        }
    }
    Class classA = A.getDataClass();
    Class classB = B.getDataClass();
    Class classC;
    if (classA == classB) {
        classC = classA;
    } else {
        throw Exception(ERROR_SAME_INTEGER_TYPE_EXPECTED);
    }
    ArrayOfMatrix m;
    ArrayOfVector v;
    v.push_back(A);
    m.push_back(v);
    v.clear();
    v.push_back(B);
    m.push_back(v);
    return ConcatenateNdArray(m, classC);
}
//=============================================================================
}
//=============================================================================
