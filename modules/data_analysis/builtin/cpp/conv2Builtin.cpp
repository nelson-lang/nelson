//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "conv2Builtin.hpp"
#include "Convolution2D.hpp"
#include "Error.hpp"
#include "OverloadFunction.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::DataAnalysisGateway::conv2Builtin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 2, 4);
    nargoutcheck(nLhs, 0, 1);
    bool bSuccess = false;
    if (eval->mustOverloadBasicTypes()) {
        retval = OverloadFunction(eval, nLhs, argIn, "conv2", bSuccess);
    }
    if (!bSuccess) {
        bool needToOverload;
        ArrayOf res;
        switch (argIn.size()) {
        case 2: {
            res = Convolution2D(argIn[0], argIn[1], L"full", needToOverload);
        } break;
        case 3: {
            if ((argIn[2].isRowVectorCharacterArray()
                    || (argIn[2].isStringArray() && argIn[2].isScalar()))) {
                std::wstring shape = argIn[2].getContentAsWideString();
                res = Convolution2D(argIn[0], argIn[1], shape, needToOverload);
            } else {
                res = Convolution2D(argIn[0], argIn[1], argIn[2], L"full", needToOverload);
            }
        } break;
        case 4: {
            std::wstring shape = argIn[3].getContentAsWideString();
            res = Convolution2D(argIn[0], argIn[1], argIn[2], shape, needToOverload);
        } break;
        default: {
            needToOverload = true;
        } break;
        }
        if (needToOverload) {
            retval = OverloadFunction(eval, nLhs, argIn, "conv2");
        } else {
            retval << res;
        }
    }
    return retval;
}
//=============================================================================
