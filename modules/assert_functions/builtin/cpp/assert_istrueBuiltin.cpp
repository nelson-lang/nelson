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
#include "assert_istrueBuiltin.hpp"
#include "Assert_IsTrue.hpp"
#include "Error.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::AssertFunctionsGateway::assert_istrueBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    if (argIn.empty() || argIn.size() > 2) {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    if (nLhs > 2) {
        Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
    ArrayOfVector retval(2);
    std::wstring modifiedmsg;
    if (argIn.size() == 2) {
        ArrayOf param2 = argIn[1];
        modifiedmsg = param2.getContentAsWideString();
    }
    ArrayOf param1 = argIn[0];
    if (!param1.isLogical()) {
        Error(ERROR_WRONG_ARGUMENT_1_TYPE_LOGICAL_EXPECTED);
    }
    if (!param1.isScalar()) {
        Error(ERROR_SIZE_SCALAR_EXPECTED);
    }
    logical res = param1.getContentAsLogicalScalar();
    std::wstring msg;
    res = Assert_IsTrue(res, modifiedmsg, msg);
    if (nLhs == 0) {
        if (res == 0) {
            Error(msg);
        }
    } else {
        retval << ArrayOf::logicalConstructor(res == 0 ? false : true);
        if (nLhs > 1) {
            retval << ArrayOf::characterArrayConstructor(msg);
        }
    }
    return retval;
}
//=============================================================================
