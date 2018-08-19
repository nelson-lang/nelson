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
#include "singleBuiltin.hpp"
#include "Error.hpp"
#include "ToSingle.hpp"
#include "OverloadFunction.hpp"
#include "OverloadRequired.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::SingleGateway::singleBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (argIn.size() != 1) {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    ArrayOf A(argIn[0]);
    // Call overload if it exists
    bool bSuccess = false;
    retval = OverloadFunction(eval, nLhs, argIn, "single", bSuccess);
    switch (A.getDataClass()) {
    case NLS_HANDLE:
    case NLS_CELL_ARRAY:
    case NLS_STRUCT_ARRAY:
    default: {
        retval = OverloadFunction(eval, nLhs, argIn, "single", bSuccess);
        if (bSuccess) {
            return retval;
        }
        retval.push_back(ToSingle(A));
    } break;
    case NLS_LOGICAL:
    case NLS_UINT8:
    case NLS_INT8:
    case NLS_UINT16:
    case NLS_INT16:
    case NLS_UINT32:
    case NLS_INT32:
    case NLS_UINT64:
    case NLS_INT64:
    case NLS_SCOMPLEX:
    case NLS_SINGLE:
    case NLS_DCOMPLEX:
    case NLS_DOUBLE:
    case NLS_CHAR:
        retval.push_back(ToSingle(A));
        break;
    }
    return retval;
}
//=============================================================================
