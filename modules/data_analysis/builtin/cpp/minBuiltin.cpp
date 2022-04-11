//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
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
Nelson::DataAnalysisGateway::minBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    bool bSuccess = false;
    nargoutcheck(nLhs, 0, 2);
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
            retval << Minimum(omitNaN, argIn[0], argIn[1], needToOverload);
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
                nargoutcheck(nLhs, 0, 1);
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
                    retval << Minimum(omitNaN, param1, param2, needToOverload);
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
                    nargoutcheck(nLhs, 0, 1);
                    retval << MinimumAll(omitNaN, param1, needToOverload);
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
                nargoutcheck(nLhs, 0, 1);
                retval << MinimumAll(omitNaN, param1, needToOverload);
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
