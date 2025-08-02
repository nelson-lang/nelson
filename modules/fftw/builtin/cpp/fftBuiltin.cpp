//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "fftBuiltin.hpp"
#include "Error.hpp"
#include "Fft.hpp"
#include "OverloadRequired.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "PredefinedErrorMessages.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
static ArrayOfVector
fftBuiltinPrivate(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);
    ArrayOf res;
    nargincheck(argIn, 1, 3);
    ArrayOf X = argIn[0];
    switch (argIn.size()) {
    case 1: {
        // fft(X)
        res = Fft(X);
    } break;
    case 2: {
        ArrayOf N = argIn[1];
        if (N.isNumeric() && N.isEmpty()) {
            // fft(X, []);
            res = Fft(X);
        } else {
            // fft(X, n)
            indexType n = N.getContentAsScalarIndex(false);
            res = Fft(X, n);
        }
    } break;
    case 3: {
        // fft(X, n, dim)
        ArrayOf N = argIn[1];
        ArrayOf DIM = argIn[2];
        indexType n;
        indexType dim = DIM.getContentAsScalarIndex(false);
        if (N.isNumeric() && N.isEmpty()) {
            // fft(X, [], dim)
            if (X.isScalar()) {
                n = 1;
            } else {
                n = X.getDimensionLength(static_cast<int>(dim) - 1);
            }
        } else {
            n = N.getContentAsScalarIndex(false);
        }
        res = Fft(X, n, dim - 1);
    } break;
    default: {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    } break;
    }
    retval << res;
    return retval;
}
//=============================================================================
ArrayOfVector
Nelson::FftwGateway::fftBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1);
    if (argIn[0].isSparse() || argIn[0].isCell() || argIn[0].isHandle() || argIn[0].isStruct()
        || argIn[0].isClassType()) {
        OverloadRequired("fft");
    }
    retval = fftBuiltinPrivate(nLhs, argIn);
    return retval;
}
//=============================================================================
