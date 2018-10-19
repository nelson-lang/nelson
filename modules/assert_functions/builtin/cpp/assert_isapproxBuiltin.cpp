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
#include "assert_isapproxBuiltin.hpp"
#include "Assert_IsApprox.hpp"
#include "Error.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::AssertFunctionsGateway::assert_isapproxBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (!((argIn.size() == 2) || (argIn.size() == 3))) {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    if (nLhs > 2) {
        Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
    double precision = 0.;
    if (argIn.size() == 3) {
        ArrayOf param3 = argIn[2];
        precision = param3.getContentAsDoubleScalar();
    }
    ArrayOf param1 = argIn[0];
    ArrayOf param2 = argIn[1];
    std::wstring msg;
    bool bRes = Assert_IsApprox(eval, param1, param2, precision, msg);
    if (nLhs == 0) {
        if (!bRes) {
            Error(msg);
        }
    } else {
        retval.push_back(ArrayOf::logicalConstructor(bRes));
        if (nLhs > 1) {
            retval.push_back(ArrayOf::characterArrayConstructor(msg));
        }
    }
    return retval;
}
//=============================================================================
