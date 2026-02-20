//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "svdBuiltin.hpp"
#include "PredefinedErrorMessages.hpp"
#include "i18n.hpp"
#include "OverloadRequired.hpp"
#include "SVD.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::LinearAlgebraGateway::svdBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 2);
    nargoutcheck(nLhs, 0, 3);
    ArrayOfVector retval(nLhs);
    if (argIn[0].isReferenceType() || argIn[0].isSparse() || argIn[0].isLogical()
        || argIn[0].isCharacterArray() || argIn[0].isIntegerType()) {
        OverloadRequired("svd");
    }
    SVD_FLAG svdFlag = SVD_FLAG::SVD_DEFAULT;
    if (argIn.size() == 2) {
        ArrayOf param2 = argIn[1];
        if (param2.isRowVectorCharacterArray()) {
            std::wstring paramAsString = param2.getContentAsWideString();
            if (L"econ" == paramAsString) {
                svdFlag = SVD_FLAG::SVD_ECON;
            } else {
                raiseError(L"Nelson:linear_algebra:ERROR_SVD_X_0_OR_SVD_X_ECON_EXPECTED",
                    ERROR_SVD_X_0_OR_SVD_X_ECON_EXPECTED);
            }
        } else {
            indexType paramAsIndex = param2.getContentAsScalarIndex(true);
            if (paramAsIndex == 0) {
                svdFlag = SVD_FLAG::SVD_0;
            } else {
                raiseError(L"Nelson:linear_algebra:ERROR_SVD_X_0_OR_SVD_X_ECON_EXPECTED",
                    ERROR_SVD_X_0_OR_SVD_X_ECON_EXPECTED);
            }
        }
    }
    switch (nLhs) {
    case 0:
    case 1: {
        ArrayOf s;
        SVD(argIn[0], s);
        retval << s;
    } break;
    case 2: {
        ArrayOf U;
        ArrayOf S;
        SVD(argIn[0], svdFlag, U, S);
        retval << U;
        retval << S;
    } break;
    case 3: {
        ArrayOf U;
        ArrayOf S;
        ArrayOf V;
        SVD(argIn[0], svdFlag, U, S, V);
        retval << U;
        retval << S;
        retval << V;
    } break;
    default:
        raiseError2(_E("nelson:arguments:wrongNumberOfOutputs"));
        break;
    }
    return retval;
}
//=============================================================================
