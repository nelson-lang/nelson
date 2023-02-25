//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "IJVBuiltin.hpp"
#include "Error.hpp"
#include "OverloadFunction.hpp"
#include "SparseType.hpp"
#include "SparseNonZeros.hpp"
#include "SparseToIJV.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "PredefinedErrorMessages.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::SparseGateway::IJVBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 6);
    nargincheck(argIn, 1, 1);
    // Call overload if it exists
    bool bSuccess = false;
    if (eval->mustOverloadBasicTypes()) {
        retval = OverloadFunction(eval, nLhs, argIn, "IJV", bSuccess);
    }
    if (!bSuccess) {
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
            retval = OverloadFunction(eval, nLhs, argIn, "IJV", bSuccess);
            if (!bSuccess) {
                Error(ERROR_WRONG_ARGUMENT_1_TYPE_SPARSE_EXPECTED);
            }
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
            if (nLhs > 4) { //-V112
                retval << N;
            }
            if (nLhs > 5) {
                retval << NNZ;
            }
        }
    }
    return retval;
}
//=============================================================================
