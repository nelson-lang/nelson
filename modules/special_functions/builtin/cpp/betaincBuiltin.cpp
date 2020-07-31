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
#include "betaincBuiltin.hpp"
#include "Error.hpp"
#include "BetaIncomplete.hpp"
#include "OverloadFunction.hpp"
#include "ClassName.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::SpecialFunctionsGateway::betaincBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (argIn.size() < 2 || argIn.size() > 4) {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    if (nLhs > 1) {
        Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
    bool bSuccess = false;
    if (eval->mustOverloadBasicTypes()) {
        retval = OverloadFunction(eval, nLhs, argIn, "betainc", bSuccess);
    }
    if (!bSuccess) {
        bool needToOverload = false;
        bool isLower = true;
        if (argIn.size() == 4) {
            std::wstring tail = argIn[3].getContentAsWideString();
            if (tail.compare(L"upper") == 0 || tail.compare(L"lower") == 0) {
                if (tail.compare(L"upper") == 0) {
                    isLower = false;
                } else {
                    isLower = true;
                }
            } else {
                Error(_("Wrong value of the fourth argument 'upper' or 'lower' expected."));
            }
        }
        retval.push_back(BetaIncomplete(argIn[0], argIn[1], argIn[2], isLower, needToOverload));
        if (needToOverload) {
            retval = OverloadFunction(eval, nLhs, argIn, "betainc");
        }
    }
    return retval;
}
//=============================================================================
