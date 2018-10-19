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
#include "assert_checkerrorBuiltin.hpp"
#include "Assert_CheckError.hpp"
#include "Error.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::AssertFunctionsGateway::assert_checkerrorBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (argIn.size() != 2) {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    if (nLhs > 2) {
        Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
    ArrayOf param1 = argIn[0];
    ArrayOf param2 = argIn[1];
    std::wstring msg = L"";
    bool res = false;
    std::wstring command = param1.getContentAsWideString();
    std::wstring expectedmsg = param2.getContentAsWideString();
    if (expectedmsg == L"") {
        Error(_W("empty string not allowed as expected message."));
    }
    res = Assert_CheckError(eval, command, expectedmsg, msg);
    if (nLhs == 0) {
        if (!res) {
            Error(msg);
        }
    } else {
        retval.push_back(ArrayOf::logicalConstructor(res));
        if (nLhs > 1) {
            retval.push_back(ArrayOf::characterArrayConstructor(msg));
        }
    }
    return retval;
}
//=============================================================================
