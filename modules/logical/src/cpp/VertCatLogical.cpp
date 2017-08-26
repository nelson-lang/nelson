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
#include <Eigen/Dense>
#include "VertCatLogical.hpp"
#include "ConcatenateNdArray.hpp"
#include "Exception.hpp"
//=============================================================================
namespace Nelson {
    //=============================================================================
    ArrayOf VertCatLogical(ArrayOf A, ArrayOf B)
    {
        if (!A.isLogical())
        {
            throw Exception(ERROR_WRONG_ARGUMENT_1_TYPE_LOGICAL_EXPECTED);
        }
        if (!B.isLogical())
        {
            throw Exception(ERROR_WRONG_ARGUMENT_2_TYPE_LOGICAL_EXPECTED);
        }
        Dimensions dimsA = A.getDimensions();
        Dimensions dimsB = B.getDimensions();
        if (!A.isEmpty(true) && !B.isEmpty(true))
        {
            if (dimsA.getColumns() != dimsB.getColumns())
            {
                throw Exception(ERROR_DIMENSIONS_NOT_CONSISTENT);
            }
        }
        Class classA = A.getDataClass();
        Class classB = B.getDataClass();
        Class classC = classA;
        indexType newColumnsSize = dimsA.getColumns();
        indexType newRowsSize = dimsA.getRows() + dimsB.getRows();
        indexType newSize = newColumnsSize * newRowsSize;
        Dimensions dimsC = Dimensions(newRowsSize, newColumnsSize);
        logical *ptrA = (logical *)A.getDataPointer();
        logical *ptrB = (logical *)B.getDataPointer();
        void * pRes = ArrayOf::allocateArrayOf(classC, newSize);
        if (newSize != 0)
        {
            logical *ptrC = (logical*)pRes;
            Eigen::Map<Eigen::Matrix<logical, Eigen::Dynamic, Eigen::Dynamic>> matA(ptrA, dimsA.getRows(), dimsA.getColumns());
            Eigen::Map<Eigen::Matrix<logical, Eigen::Dynamic, Eigen::Dynamic>> matB(ptrB, dimsB.getRows(), dimsB.getColumns());
            Eigen::Map<Eigen::Matrix<logical, Eigen::Dynamic, Eigen::Dynamic>> matC(ptrC, dimsC.getRows(), dimsC.getColumns());
            matC << matA,
                 matB;
        }
        return ArrayOf(classC, dimsC, pRes);
    }
    //=============================================================================
    ArrayOf VertCatNdArrayLogical(ArrayOf A, ArrayOf B)
    {
        if (!A.isNdArrayLogical())
        {
            throw Exception(ERROR_WRONG_ARGUMENT_1_TYPE_LOGICAL_EXPECTED);
        }
        if (!B.isNdArrayLogical())
        {
            throw Exception(ERROR_WRONG_ARGUMENT_2_TYPE_LOGICAL_EXPECTED);
        }
        Dimensions dimsA = A.getDimensions();
        Dimensions dimsB = B.getDimensions();
        if (dimsA.getColumns() != dimsB.getColumns())
        {
            throw Exception(ERROR_DIMENSIONS_NOT_CONSISTENT);
        }
        if (dimsA.getLength() != dimsB.getLength())
        {
            throw Exception(ERROR_DIMENSIONS_NOT_CONSISTENT);
        }
        for (indexType k = 0; k < dimsA.getLength(); k++)
        {
            if (k != 0)
            {
                if (dimsA.getDimensionLength(k) != dimsB.getDimensionLength(k))
                {
                    throw Exception(ERROR_DIMENSIONS_NOT_CONSISTENT);
                }
            }
        }
        Class classA = A.getDataClass();
        Class classB = B.getDataClass();
        Class classC = classA;
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
