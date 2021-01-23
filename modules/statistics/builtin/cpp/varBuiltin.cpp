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
#include "varBuiltin.hpp"
#include "Error.hpp"
#include "OverloadFunction.hpp"
#include "OverloadRequired.hpp"
#include "Variance.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::StatisticsGateway::varBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    bool bSuccess = false;
    bool nArgInSupported = argIn.size() > 0 && argIn.size() < 4;
    if (!nArgInSupported) {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    if (nLhs > 1) {
        Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
    if (eval->mustOverloadBasicTypes()) {
        retval = OverloadFunction(eval, nLhs, argIn, "var", bSuccess);
    }
    if (!bSuccess) {
        ArrayOf A = argIn[0];
        int w = 0;
        int d = -1;
        if (argIn.size() > 1) {
            ArrayOf arg2 = argIn[1];
            w = (int)arg2.getContentAsScalarIndex(true);
            bool wValid = (w == 0 || w == 1);
            if (!wValid) {
                Error(_W("Wrong value for #2 argument."));
            }
        }
        if (argIn.size() > 2) {
            ArrayOf arg3 = argIn[2];
            d = (int)arg3.getContentAsScalarIndex(true);
            if (d <= 0) {
                Error(_W("Wrong value for #3 argument."));
            }
        }
        bool needToOverload = false;
        ArrayOf res = Variance(A, w, d, needToOverload);
        if (needToOverload) {
            retval = OverloadFunction(eval, nLhs, argIn, "var");
        } else {
            retval.push_back(res);
        }
    }
    return retval;
}
//=============================================================================
