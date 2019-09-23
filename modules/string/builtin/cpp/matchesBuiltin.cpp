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
#include "matchesBuiltin.hpp"
#include "Error.hpp"
#include "StringMatches.hpp"
#include "OverloadFunction.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::StringGateway::matchesBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (nLhs > 1) {
        Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
    if (argIn.size() != 2 && argIn.size() != 4) {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }

    bool ignoreCase = false;
    if (argIn.size() == 4) {
        ArrayOf param3 = argIn[2];
        std::wstring fieldname = param3.getContentAsWideString();
        if (fieldname != L"IgnoreCase") {
            Error(_W("Wrong value for #3: 'IgnoreCase' expected."));
        }
        ArrayOf param4 = argIn[3];
        ignoreCase = param4.getContentAsLogicalScalar();
    }

    ArrayOf A = argIn[0];
    ArrayOf B = argIn[1];
    // Call overload if it exists
    bool bSuccess = false;
    if (eval->mustOverloadBasicTypes()) {
        retval = OverloadFunction(eval, nLhs, argIn, "matches", bSuccess);
    }
    if (!bSuccess) {
        ArrayOf A = argIn[0];
        ArrayOf B = argIn[1];
        retval.push_back(StringMatches(A, B, ignoreCase));
    }
    return retval;
}
//=============================================================================
