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
#include "isequaltoBuiltin.hpp"
#include "Error.hpp"
#include "OverloadFunction.hpp"
#include "OverloadRequired.hpp"
#include "IsEqual.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::ElementaryFunctionsGateway::isequaltoBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (nLhs > 1) {
        Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
    if (argIn.size() < 2) {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    bool bSuccess = false;
    if (eval->mustOverloadBasicTypes()) {
        retval = OverloadFunction(eval, nLhs, argIn, "isequalto", bSuccess);
    }
    if (!bSuccess) {
        bool res = false;
        for (size_t k = 1; k < argIn.size(); k++) {
            bool needToOverload = false;
            ArrayOf param1 = argIn[k - 1];
            ArrayOf param2 = argIn[k];
            res = IsEqual(param1, param2, true, true, needToOverload);
            if (needToOverload) {
                ArrayOfVector v1v2;
                v1v2.push_back(param1);
                v1v2.push_back(param2);
                ArrayOfVector ret = OverloadFunction(eval, nLhs, v1v2, "isequalto", bSuccess);
                {
                    if (ret.size() == 1) {
                        res = ret[0].getContentAsLogicalScalar(false) == 0 ? false : true;
                        if (!res) {
                            retval.push_back(ArrayOf::logicalConstructor(res));
                            return retval;
                        }
                    } else {
                        Error(_W("overload of isequalto must return a logical."));
                    }
                }
            } else {
                if (!res) {
                    retval.push_back(ArrayOf::logicalConstructor(res));
                    return retval;
                }
            }
        }
        retval.push_back(ArrayOf::logicalConstructor(res));
    }
    return retval;
}
//=============================================================================
