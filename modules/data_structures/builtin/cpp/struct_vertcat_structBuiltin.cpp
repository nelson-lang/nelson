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
#include "struct_vertcat_structBuiltin.hpp"
#include "Error.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::DataStructuresGateway::struct_vertcat_structBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (argIn.size() != 2) {
        Error(eval, ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    if (nLhs > 1) {
        Error(eval, ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
    ArrayOf A = argIn[0];
    ArrayOf B = argIn[1];
    ArrayOf C;
    if (!A.isStruct()) {
        Error(eval, ERROR_WRONG_ARGUMENT_1_TYPE_STRUCT_EXPECTED);
    }
    if (!B.isStruct()) {
        Error(eval, ERROR_WRONG_ARGUMENT_2_TYPE_STRUCT_EXPECTED);
    }
    stringVector fieldnamesA = A.getFieldNames();
    stringVector fieldnamesB = B.getFieldNames();
    if (fieldnamesA.size() != fieldnamesB.size()) {
        Error(eval, ERROR_FIELDNAMES_MUST_MATCH);
    }
    for (size_t k = 0; k < fieldnamesA.size(); k++) {
        if (fieldnamesA[k] != fieldnamesB[k]) {
            Error(eval, ERROR_FIELDNAMES_MUST_MATCH);
        }
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
        Error(eval, ERROR_DIMENSIONS_NOT_CONSISTENT);
    }
    indexType newColumnsSize = dimsA.getColumns();
    indexType newRowsSize = dimsA.getRows() + dimsB.getRows();
    indexType newSize = newColumnsSize * newRowsSize;
    Dimensions dimsC = Dimensions(newRowsSize, newColumnsSize);
    void* res = ArrayOf::allocateArrayOf(NLS_STRUCT_ARRAY, newSize, fieldnamesA);
    ArrayOf* elements = (ArrayOf*)res;
    C = ArrayOf(NLS_STRUCT_ARRAY, dimsC, elements, false, fieldnamesA);
    for (size_t k = 0; k < fieldnamesA.size(); k++) {
        ArrayOfVector fieldsA = A.getFieldAsList(fieldnamesA[k]);
        ArrayOfVector fieldsB = B.getFieldAsList(fieldnamesA[k]);
        ArrayOfVector fieldsC = fieldsA;
        fieldsC.insert(fieldsC.end(), fieldsB.begin(), fieldsB.end());
        C.setFieldAsList(fieldnamesA[k], fieldsC);
    }
    retval.push_back(C);
    return retval;
}
//=============================================================================
