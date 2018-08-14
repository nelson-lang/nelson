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
#include "cell_vertcat_cellBuiltin.hpp"
#include "Error.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::DataStructuresGateway::cell_vertcat_cellBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (argIn.size() != 2) {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    if (nLhs > 1) {
        Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
    ArrayOf A = argIn[0];
    ArrayOf B = argIn[1];
    ArrayOf C;
    if (!A.isCell()) {
        Error(ERROR_WRONG_ARGUMENT_1_TYPE_CELL_EXPECTED);
    }
    if (!B.isCell()) {
        Error(ERROR_WRONG_ARGUMENT_2_TYPE_CELL_EXPECTED);
    }
    if (A.isEmpty()) {
        C = B;
        C.ensureSingleOwner();
        retval.push_back(C);
        return retval;
    }
    if (B.isEmpty()) {
        C = A;
        C.ensureSingleOwner();
        retval.push_back(C);
        return retval;
    }
    Dimensions dimsA = A.getDimensions();
    Dimensions dimsB = B.getDimensions();
    if (dimsA.getColumns() != dimsB.getColumns()) {
        Error(ERROR_DIMENSIONS_NOT_CONSISTENT);
    }
    indexType newColumnsSize = dimsA.getColumns();
    indexType newRowsSize = dimsA.getRows() + dimsB.getRows();
    indexType newSize = newColumnsSize * newRowsSize;
    Dimensions dimsC = Dimensions(newRowsSize, newColumnsSize);
    void* res = ArrayOf::allocateArrayOf(NLS_CELL_ARRAY, newSize);
    ArrayOf* elements = (ArrayOf*)res;
    ArrayOf* elementsA = (ArrayOf*)A.getDataPointer();
    ArrayOf* elementsB = (ArrayOf*)B.getDataPointer();
    for (indexType i = 0; i < dimsC.getRows(); i++) {
        for (indexType j = 0; j < dimsC.getColumns(); j++) {
            indexType idxDST = i + j * dimsC.getRows();
            indexType idxSRC = 0;
            if (i < dimsA.getRows()) {
                idxSRC = i + j * dimsA.getRows();
                elements[idxDST] = elementsA[idxSRC];
            } else {
                idxSRC = (i - dimsA.getRows()) + j * dimsA.getRows();
                elements[idxDST] = elementsB[idxSRC];
            }
        }
    }
    C = ArrayOf(NLS_CELL_ARRAY, dimsC, elements);
    retval.push_back(C);
    return retval;
}
//=============================================================================
