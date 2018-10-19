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
#include "struct2cellBuiltin.hpp"
#include "Error.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::DataStructuresGateway::struct2cellBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector ret;
    if (nLhs > 1) {
        Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
    if (argIn.size() != 1) {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    ArrayOf param1 = argIn[0];
    if (!param1.isStruct()) {
        Error(ERROR_WRONG_ARGUMENT_1_TYPE_STRUCT_EXPECTED);
    }
    stringVector fieldnames = param1.getFieldNames();
    size_t nbFields = fieldnames.size();
    ArrayOf* elements = nullptr;
    Dimensions dimsStruct = param1.getDimensions();
    dimsStruct.simplify();
    try {
        indexType nbElements = dimsStruct.getElementCount() * nbFields;
        elements = new ArrayOf[nbElements];
    } catch (const std::bad_alloc& e) {
        e.what();
        Error(ERROR_MEMORY_ALLOCATION);
    }
    try {
        ArrayOfVector* v = new ArrayOfVector[nbFields];
        indexType S = 0;
        for (size_t k = 0; k < nbFields; k++) {
            v[k] = param1.getFieldAsList(fieldnames[k]);
            S = v[k].size();
        }
        size_t l = 0;
        for (size_t q = 0; q < S; q++) {
            for (size_t k = 0; k < nbFields; k++) {
                elements[l] = v[k][q];
                l++;
            }
        }
        delete[] v;
    } catch (const std::bad_alloc& e) {
        delete[] elements;
        e.what();
        Error(ERROR_MEMORY_ALLOCATION);
    }
    Dimensions dims;
    dims[0] = nbFields;
    for (size_t k = 0; k < dimsStruct.getLength(); k++) {
        dims[k + 1] = dimsStruct[k];
    }
    dims.simplify();
    ret.push_back(ArrayOf(NLS_CELL_ARRAY, dims, elements));
    return ret;
}
//=============================================================================
