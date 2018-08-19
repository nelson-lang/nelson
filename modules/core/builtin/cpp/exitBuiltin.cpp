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
#include "exitBuiltin.hpp"
#include "Error.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::CoreGateway::exitBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (argIn.size() > 1) {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    int iValue = 0;
    if (argIn.size() == 0) {
        iValue = 0;
    } else {
        ArrayOf param1 = argIn[0];
        if (!param1.isDoubleType() || (param1.isSparse())) {
            Error(ERROR_WRONG_ARGUMENT_1_TYPE_DOUBLE_EXPECTED);
        } else {
            ArrayOf param1 = argIn[0];
            if (!param1.isScalar()) {
                Error(ERROR_WRONG_ARGUMENT_1_SIZE_SCALAR_EXPECTED);
            }
            double dValue = param1.getContentAsDoubleScalar();
            iValue = (int)dValue;
            if ((double)iValue != dValue) {
                Error(ERROR_WRONG_ARGUMENT_1_SCALAR_INTEGER_VALUE_EXPECTED);
            }
        }
    }
    eval->setExitCode(iValue);
    eval->setState(NLS_STATE_QUIT);
    return retval;
}
//=============================================================================
