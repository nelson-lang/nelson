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
#include "traceBuiltin.hpp"
#include "Error.hpp"
#include "OverloadFunction.hpp"
#include "OverloadRequired.hpp"
#include "TraceMatrix.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::LinearAlgebraGateway::traceBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (argIn.size() != 1) {
        Error(eval, ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    if (nLhs > 1) {
        Error(eval, ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
    // Call overload if it exists
    bool bSuccess = false;
    if (eval->overloadOnBasicTypes) {
        retval = OverloadFunction(eval, nLhs, argIn, "trace", bSuccess);
    }
    if (!bSuccess) {
        if ((argIn[0].getDataClass() == NLS_STRUCT_ARRAY)
            || (argIn[0].getDataClass() == NLS_CELL_ARRAY) || argIn[0].isSparse()
            || argIn[0].isLogical() || argIn[0].isCharacterArray() || argIn[0].isIntegerType()
            || argIn[0].isHandle()) {
            retval = OverloadFunction(eval, nLhs, argIn, "trace", bSuccess);
            if (bSuccess) {
                return retval;
            }
            OverloadRequired(eval, argIn, Overload::OverloadClass::FUNCTION);
        }
        retval.push_back(TraceMatrix(argIn[0]));
    }
    return retval;
}
//=============================================================================
