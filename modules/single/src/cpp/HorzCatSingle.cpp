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
#include "HorzCatSingle.hpp"
#include "ConcatenateNdArray.hpp"
#include "Exception.hpp"
#include <Eigen/Dense>
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
HorzCatSingle(ArrayOf A, ArrayOf B)
{
    if (!A.isSingleType()) {
        throw Exception(ERROR_WRONG_ARGUMENT_1_TYPE_SINGLE_EXPECTED);
    }
    if (!B.isSingleType()) {
        throw Exception(ERROR_WRONG_ARGUMENT_2_TYPE_SINGLE_EXPECTED);
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
    if (dimsA.getRows() != dimsB.getRows()) {
        throw Exception(ERROR_DIMENSIONS_NOT_CONSISTENT);
    }
    Class classA = A.getDataClass();
    Class classB = B.getDataClass();
    Class classC;
    if (classA == classB) {
        classC = classA;
    } else {
        classC = NLS_SCOMPLEX;
        A.promoteType(NLS_SCOMPLEX);
        B.promoteType(NLS_SCOMPLEX);
    }
    indexType newColumnsSize = dimsA.getColumns() + dimsB.getColumns();
    indexType newRowsSize = dimsA.getRows();
    indexType newSize = newColumnsSize * newRowsSize;
    Dimensions dimsC = Dimensions(newRowsSize, newColumnsSize);
    void* pRes = nullptr;
    single* ptrC = nullptr;
    single* ptrA = (single*)A.getDataPointer();
    single* ptrB = (single*)B.getDataPointer();
    if (classC == NLS_SCOMPLEX) {
        pRes = ArrayOf::allocateArrayOf(classC, newSize * 2);
        ptrC = (single*)pRes;
        singlecomplex* Cz = reinterpret_cast<singlecomplex*>(ptrC);
        Eigen::Map<Eigen::MatrixXcf> matC(Cz, dimsC.getRows(), dimsC.getColumns());
        singlecomplex* Az = reinterpret_cast<singlecomplex*>(ptrA);
        Eigen::Map<Eigen::MatrixXcf> matA(Az, dimsA.getRows(), dimsA.getColumns());
        singlecomplex* Bz = reinterpret_cast<singlecomplex*>(ptrB);
        Eigen::Map<Eigen::MatrixXcf> matB(Bz, dimsB.getRows(), dimsB.getColumns());
        matC << matA, matB;
    } else {
        pRes = ArrayOf::allocateArrayOf(classC, newSize);
        ptrC = (single*)pRes;
        Eigen::Map<Eigen::Matrix<single, Eigen::Dynamic, Eigen::Dynamic>> matA(
            ptrA, dimsA.getRows(), dimsA.getColumns());
        Eigen::Map<Eigen::Matrix<single, Eigen::Dynamic, Eigen::Dynamic>> matB(
            ptrB, dimsB.getRows(), dimsB.getColumns());
        Eigen::Map<Eigen::Matrix<single, Eigen::Dynamic, Eigen::Dynamic>> matC(
            ptrC, dimsC.getRows(), dimsC.getColumns());
        matC << matA, matB;
    }
    return ArrayOf(classC, dimsC, pRes);
}
//=============================================================================
ArrayOf
HorzCatNdArraySingle(ArrayOf A, ArrayOf B)
{
    if (!A.isNdArraySingleType()) {
        throw Exception(ERROR_WRONG_ARGUMENT_1_TYPE_SINGLE_EXPECTED);
    }
    if (!B.isNdArraySingleType()) {
        throw Exception(ERROR_WRONG_ARGUMENT_2_TYPE_SINGLE_EXPECTED);
    }
    Dimensions dimsA = A.getDimensions();
    Dimensions dimsB = B.getDimensions();
    if (dimsA.getRows() != dimsB.getRows()) {
        throw Exception(ERROR_DIMENSIONS_NOT_CONSISTENT);
    }
    if (dimsA.getLength() != dimsB.getLength()) {
        throw Exception(ERROR_DIMENSIONS_NOT_CONSISTENT);
    }
    for (indexType k = 0; k < dimsA.getLength(); k++) {
        if (k != 1) {
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
        classC = NLS_SCOMPLEX;
        A.promoteType(NLS_SCOMPLEX);
        B.promoteType(NLS_SCOMPLEX);
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
