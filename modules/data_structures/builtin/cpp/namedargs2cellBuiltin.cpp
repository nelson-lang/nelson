//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// Alternatively, you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of
// the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this program. If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#include "namedargs2cellBuiltin.hpp"
#include "Error.hpp"
#include "OverloadFunction.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::DataStructuresGateway::namedargs2cellBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (nLhs > 1) {
        Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
    bool bSuccess = false;
    if (eval->mustOverloadBasicTypes()) {
        retval = OverloadFunction(eval, nLhs, argIn, "namedargs2cell", bSuccess);
    }
    if (!bSuccess) {
        if (argIn.size() != 1) {
            Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
        }
        ArrayOf param1 = argIn[0];
        if (!param1.isStruct()) {
            Error(ERROR_WRONG_ARGUMENT_1_TYPE_STRUCT_EXPECTED);
        }
        if (!param1.isScalar()) {
            Error(ERROR_WRONG_ARGUMENT_1_SIZE_SCALAR_EXPECTED);
        }
        stringVector fieldnames = param1.getFieldNames();
        ArrayOf* elements
            = (ArrayOf*)ArrayOf::allocateArrayOf(NLS_CELL_ARRAY, fieldnames.size() * 2);
        Dimensions dims(1, fieldnames.size() * 2);
        ArrayOf res = ArrayOf(NLS_CELL_ARRAY, dims, elements);
        indexType k = 0;
        for (std::string name : fieldnames) {
            elements[k] = ArrayOf::characterArrayConstructor(name);
            elements[k + 1] = param1.getField(name);
            k = k + 2;
        }
        retval.push_back(res);
    }
    return retval;
}
//=============================================================================
