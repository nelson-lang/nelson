//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "IJVBuiltin.hpp"
#include "Error.hpp"
#include "SparseType.hpp"
#include "SparseNonZeros.hpp"
#include "SparseToIJV.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "PredefinedErrorMessages.hpp"
#include "OverloadRequired.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::SparseGateway::IJVBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 6);
    nargincheck(argIn, 1, 1);

    ArrayOf A(argIn[0]);
    ArrayOf I;
    ArrayOf J;
    ArrayOf V;
    ArrayOf M;
    ArrayOf N;
    ArrayOf NNZ;
    bool needToOverload;
    SparseToIJV(A, I, J, V, M, N, NNZ, needToOverload);
    if (needToOverload) {
        OverloadRequired("ijv");
    } else {
        retval << I;
        if (nLhs > 1) {
            retval << J;
        }
        if (nLhs > 2) {
            retval << V;
        }
        if (nLhs > 3) {
            retval << M;
        }
        if (nLhs > 4) {
            retval << N;
        }
        if (nLhs > 5) {
            retval << NNZ;
        }
    }
    return retval;
}
//=============================================================================
