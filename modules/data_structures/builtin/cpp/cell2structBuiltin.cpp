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
#include "cell2structBuiltin.hpp"
#include "Error.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::DataStructuresGateway::cell2structBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector ret;
    if (nLhs > 1) {
        Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
    if ((argIn.size() > 3) || (argIn.size() < 2)) {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    ArrayOf param1 = argIn[0];
    if (!param1.isCell()) {
        Error(ERROR_WRONG_ARGUMENT_1_TYPE_CELL_EXPECTED);
    }
    ArrayOf param2 = argIn[1];
    stringVector fieldnames = param2.getContentAsCStringVector(false);
    indexType dim = 0;
    if (argIn.size() == 2) {
        dim = 0;
    } else {
        ArrayOf param3 = argIn[2];
        dim = param3.getContentAsScalarIndex(false) - 1;
    }
    if (dim > 1) {
        Error(_W("Not yet implemented with dim > 2"));
    }
    Dimensions dims1 = param1.getDimensions();
    Dimensions dims2 = param2.getDimensions();
    //    if (fieldnames.size() != 1)
    if (!param1.isEmpty()) {
        if (dims1[dim] != fieldnames.size()) {
            Error(_W("Number of field names must match number of fields in new structure."));
        }
    }
    ArrayOf* arg = (ArrayOf*)(param1.getDataPointer());
    if (dim == 0) {
        if (param1.isEmpty()) {
            indexType len = std::min(dims1.getLength(), dims2.getLength());
            Dimensions dims;
            if (dims1.equals(dims2)) {
                indexType l = 0;
                for (indexType k = dim + 1; k < dims1.getLength(); k++) {
                    dims[l] = dims1[k];
                    l++;
                }
                if (dims.getLength() < len) {
                    dims[dims.getLength()] = 1;
                }
                dims.simplify();
            } else if (dims1.getLength() > dims2.getLength()) {
                indexType l = 0;
                for (indexType k = dim + 1; k < dims1.getLength(); k++) {
                    dims[l] = dims1[k];
                    l++;
                }
            } else {
                if (dims1[dim] == 0) {
                    dims[0] = 1;
                } else {
                    dims[0] = dims1.getElementCount() / dims1[dim];
                }
                dims[1] = 1;
            }
            if (dims.isScalar()) {
                ret.push_back(ArrayOf::emptyStructWithoutFields());
            } else if (dims.getElementCount() == 0) {
                ArrayOf c = ArrayOf::emptyConstructor(dims);
                c.promoteType(NLS_STRUCT_ARRAY);
                ret.push_back(c);
            } else {
                ArrayOf c = ArrayOf::emptyStructConstructor(fieldnames, dims);
                ret.push_back(c);
            }
        } else {
            Dimensions dims;
            dims[1] = 1;
            dims[0] = dims1.getElementCount() / dims1[dim];
            ArrayOf* qp = (ArrayOf*)ArrayOf::allocateArrayOf(
                NLS_STRUCT_ARRAY, dims.getElementCount(), fieldnames);
            ArrayOf c = ArrayOf(NLS_STRUCT_ARRAY, dims, qp, false, fieldnames);
            for (size_t k = 0; k < param1.getDimensions().getElementCount(); k++) {
                qp[k] = arg[k];
            }
            ret.push_back(c);
        }
    } else /* dim == 1 */
    {
        Dimensions dims;
        dims[0] = dims1.getElementCount() / dims1[dim];
        dims[1] = 1;
        ArrayOf* qp = (ArrayOf*)ArrayOf::allocateArrayOf(
            NLS_STRUCT_ARRAY, dims.getElementCount(), fieldnames);
        size_t l = 0;
        indexType rowCount = param1.getDimensions()[0];
        indexType colCount = param1.getDimensions()[1];
        for (size_t i = 0; i < rowCount; i++) {
            for (size_t j = 0; j < colCount; j++) {
                qp[l] = arg[i + j * rowCount];
                l++;
            }
        }
        ArrayOf c = ArrayOf(NLS_STRUCT_ARRAY, dims, qp, false, fieldnames);
        ret.push_back(c);
    }
    return ret;
}
//=============================================================================
