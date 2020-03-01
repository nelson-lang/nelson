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
#include "minBuiltin.hpp"
#include "Error.hpp"
#include "OverloadFunction.hpp"
#include "OverloadRequired.hpp"
#include "Minimum.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::ElementaryFunctionsGateway::minBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    bool bSuccess = false;
    if (nLhs > 2) {
        Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
    if (eval->mustOverloadBasicTypes()) {
        retval = OverloadFunction(eval, nLhs, argIn, "min", bSuccess);
    }
    if (!bSuccess) {
        bool omitNaN = true;
        bool needToOverload = false;
        switch (argIn.size()) {
        case 1: {
            // M = min(A);
            // [M, i] = min(A);
            retval = Minimum(omitNaN, argIn[0], nLhs, needToOverload);
        } break;
        case 2: {
            // C = min(A, B)
            ArrayOf res = Minimum(omitNaN, argIn[0], argIn[1], needToOverload);
            retval.push_back(res);
        } break;
        case 3: {
            // [M, I] = min(A, [], dim)
            // [M, I] = min(A, [], nanflag)
            // M = min(A, B, nanflag)
            // M = min(A, [], 'all')
            ArrayOf param1 = argIn[0];
            ArrayOf param2 = argIn[1];
            ArrayOf param3 = argIn[2];
            Dimensions dimsA = param1.getDimensions();
            Dimensions dimsB = param2.getDimensions();
            if (dimsA.equals(dimsB)) {
                if (nLhs > 1) {
                    Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
                }
                if (param3.isRowVectorCharacterArray()
                    || (param3.isStringArray() && param3.isScalar())) {
                    std::wstring s = param3.getContentAsWideString();
                    if (s == L"omitnan") {
                        omitNaN = true;
                    } else if (s == L"includenan") {
                        omitNaN = false;
                    } else {
                        Error(_("Invalid third argument."));
                    }
                    ArrayOf res = Minimum(omitNaN, param1, param2, needToOverload);
                    retval.push_back(res);
                } else {
                    Error(_("Invalid third argument."));
                }
            } else {
                if (!param2.isEmpty()) {
                    Error(_("Invalid second argument."));
                }
                indexType dim = 0;
                bool isAll = false;
                if (param3.isRowVectorCharacterArray()
                    || (param3.isStringArray() && param3.isScalar())) {
                    std::wstring s = param3.getContentAsWideString();
                    if (s == L"omitnan") {
                        omitNaN = true;
                    } else if (s == L"includenan") {
                        omitNaN = false;
                    } else if (s == L"all") {
                        isAll = true;
                    } else {
                        Error(_("Invalid third argument."));
                    }
                } else {
                    dim = param3.getContentAsScalarIndex(false);
                }
                if (isAll) {
                    if (nLhs > 1) {
                        Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
                    }
                    ArrayOf res = MinimumAll(omitNaN, param1, needToOverload);
                    retval.push_back(res);
                } else if (dim == 0) {
                    retval = Minimum(omitNaN, param1, nLhs, needToOverload);
                } else {
                    retval = Minimum(omitNaN, param1, dim, nLhs, needToOverload);
                }
            }
        } break;
        case 4: {
            // [M, I] = min(A, [], dim, nanflag)
            // M = min(A, [], 'all', nanflag)
            ArrayOf param4 = argIn[3];
            if (param4.isRowVectorCharacterArray()
                || (param4.isStringArray() && param4.isScalar())) {
                std::wstring s = param4.getContentAsWideString();
                if (s == L"omitnan") {
                    omitNaN = true;
                } else if (s == L"includenan") {
                    omitNaN = false;
                } else {
                    Error(_("Invalid 4th argument."));
                }
            }
            ArrayOf param1 = argIn[0];
            ArrayOf param2 = argIn[1];
            ArrayOf param3 = argIn[2];
            if (!param2.isEmpty()) {
                Error(_("Invalid second argument."));
            }
            if (param3.isRowVectorCharacterArray()
                || (param3.isStringArray() && param3.isScalar())) {
                std::wstring s = param3.getContentAsWideString();
                if (s != L"all") {
                    Error(_("Invalid third argument."));
                }
                if (nLhs > 1) {
                    Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
                }
                ArrayOf res = MinimumAll(omitNaN, param1, needToOverload);
                retval.push_back(res);
            } else {
                indexType dim = param3.getContentAsScalarIndex(false);
                retval = Minimum(omitNaN, param1, dim, nLhs, needToOverload);
            }
        } break;
        default: {
            Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
        } break;
        }
        if (needToOverload) {
            retval = OverloadFunction(eval, nLhs, argIn, "min");
        }
    }
    return retval;
}
//=============================================================================
