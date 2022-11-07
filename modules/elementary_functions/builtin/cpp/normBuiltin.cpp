//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "normBuiltin.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "OverloadFunction.hpp"
#include "Norm.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::ElementaryFunctionsGateway::normBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 2);
    nargoutcheck(nLhs, 0, 1);
    bool bSuccess = false;
    if (eval->mustOverloadBasicTypes()) {
        retval = OverloadFunction(eval, nLhs, argIn, "norm", bSuccess);
    }
    if (!bSuccess) {
        if (argIn[0].getDataClass() == NLS_SINGLE || argIn[0].getDataClass() == NLS_SCOMPLEX
            || argIn[0].getDataClass() == NLS_DOUBLE || argIn[0].getDataClass() == NLS_DCOMPLEX) {
            if (argIn[0].isSparse()) {
                retval = OverloadFunction(eval, nLhs, argIn, "norm", bSuccess);
                if (!bSuccess) {
                    Error(_W("Sparse not supported."));
                }
            } else {
                if (argIn.size() > 1) {
                    if (argIn[1].isRowVectorCharacterArray()) {
                        std::wstring param = argIn[1].getContentAsWideString();
                        if (param == L"fro") {
                            retval << NormFrobenius(argIn[0]);
                        } else {
                            Error(ERROR_WRONG_ARGUMENT_2_VALUE);
                        }
                    } else {
                        ArrayOf param = argIn[1];
                        double p = param.getContentAsDoubleScalar();
                        retval << Norm(argIn[0], p);
                    }
                } else {
                    retval << Norm(argIn[0], 2);
                }
            }
        } else {
            retval = OverloadFunction(eval, nLhs, argIn, "norm", bSuccess);
            if (!bSuccess) {
                Error(ERROR_WRONG_ARGUMENT_1_TYPE);
            }
        }
    }
    return retval;
}
//=============================================================================
