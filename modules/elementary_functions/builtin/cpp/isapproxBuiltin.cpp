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
#include "isapproxBuiltin.hpp"
#include "Error.hpp"
#include "IsApprox.hpp"
#include "OverloadFunction.hpp"
#include "OverloadRequired.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::ElementaryFunctionsGateway::isapproxBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (!(argIn.size() == 2 || argIn.size() == 3)) {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    if (nLhs > 1) {
        Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
    bool bSuccess = false;
    if (eval->mustOverloadBasicTypes()) {
        retval = OverloadFunction(eval, nLhs, argIn, "isapprox", bSuccess);
    }
    if (!bSuccess) {
        if (argIn[0].isSparse() || argIn[0].isCell() || argIn[0].isHandle() || argIn[0].isStruct()
            || argIn[0].isClassStruct()) {
            retval = OverloadFunction(eval, nLhs, argIn, "isapprox", bSuccess);
            if (bSuccess) {
                return retval;
            }
        }
        double precision = 0.;
        if (argIn.size() == 3) {
            ArrayOf param3 = argIn[2];
            precision = param3.getContentAsDoubleScalar();
        }
        ArrayOf param1 = argIn[0];
        ArrayOf param2 = argIn[1];
        if (param1.isNumeric() && param2.isNumeric()) {
            if (param1.isSparse() || param2.isSparse()) {
                Error(_W("Sparse type not supported."));
            }
            if (param1.isComplex() || param2.isComplex()) {
                param1.promoteType(NLS_DCOMPLEX);
                param2.promoteType(NLS_DCOMPLEX);
            } else {
                param1.promoteType(NLS_DOUBLE);
                param2.promoteType(NLS_DOUBLE);
            }
            retval.push_back(ArrayOf::logicalConstructor(IsApprox(param1, param2, precision)));
        } else {
            Error(_W("Numerics types expected."));
        }
    }
    return retval;
}
//=============================================================================
