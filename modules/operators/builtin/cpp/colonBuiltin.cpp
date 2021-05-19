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
#include "colonBuiltin.hpp"
#include "Error.hpp"
#include "OverloadBinaryOperator.hpp"
#include "OverloadTernaryOperator.hpp"
#include "Colon.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::OperatorsGateway::colonBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    ArrayOf res;
    if (argIn.size() == 2) {
        ArrayOf A = argIn[0];
        ArrayOf B = argIn[1];
        res = eval->colonUnitOperator(argIn[0], argIn[1]);
    } else if (argIn.size() == 3) {
        ArrayOf A = argIn[0];
        ArrayOf B = argIn[1];
        ArrayOf C = argIn[2];
        res = eval->colonOperator(argIn[0], argIn[1], argIn[2]);
    } else {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    retval << res;
    return retval;
}
//=============================================================================
