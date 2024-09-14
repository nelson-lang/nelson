//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <string>
#include "nlsBuildConfig.h"
#include "datevecBuiltin.hpp"
#include "Error.hpp"
#include "DateVector.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "OverloadRequired.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::TimeGateway::datevecBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1);
    nargoutcheck(nLhs, 0, 6);
    ArrayOfVector retval(nLhs);
    nargincheck(argIn, 1, 1);
    ArrayOf param1 = argIn[0];
    if (((param1.getDataClass() == NLS_DOUBLE) || (param1.getDataClass() == NLS_DCOMPLEX))
        && (!param1.isSparse())) {
        if (param1.getDataClass() == NLS_DCOMPLEX) {
            param1.promoteType(NLS_DOUBLE);
        }
        indexType len = param1.getElementCount();
        auto* ptd = (double*)param1.getDataPointer();
        if (nLhs < 2) {
            double* res = static_cast<double*>(
                ArrayOf::allocateArrayOf(NLS_DOUBLE, 6 * len, stringVector(), false));

#if WITH_OPENMP
#pragma omp parallel for
#endif
            for (ompIndexType k = 0; k < (ompIndexType)len; k++) {
                double DT, Y, M, D, H, MN, S, MS;
                DT = ptd[k];
                DateVector(DT, Y, M, D, H, MN, S, MS);
                res[k] = Y;
                res[k + len] = M;
                res[k + len * 2] = D;
                res[k + len * 3] = H;
                res[k + len * 4] = MN;
                res[k + len * 5] = S;
            }
            Dimensions dim(len, 6);
            retval << ArrayOf(NLS_DOUBLE, dim, res);
        } else {
            double* Y = nullptr;
            double* M = nullptr;
            double* D = nullptr;
            double* H = nullptr;
            double* MN = nullptr;
            double* S = nullptr;

            Y = static_cast<double*>(
                ArrayOf::allocateArrayOf(NLS_DOUBLE, len, stringVector(), false));
            M = static_cast<double*>(
                ArrayOf::allocateArrayOf(NLS_DOUBLE, len, stringVector(), false));
            if (nLhs > 2) {
                D = static_cast<double*>(
                    ArrayOf::allocateArrayOf(NLS_DOUBLE, len, stringVector(), false));
            }
            if (nLhs > 3) {
                H = static_cast<double*>(
                    ArrayOf::allocateArrayOf(NLS_DOUBLE, len, stringVector(), false));
            }
            if (nLhs > 4) {
                MN = static_cast<double*>(
                    ArrayOf::allocateArrayOf(NLS_DOUBLE, len, stringVector(), false));
            }
            if (nLhs > 5) {
                S = static_cast<double*>(
                    ArrayOf::allocateArrayOf(NLS_DOUBLE, len, stringVector(), false));
            }

#if WITH_OPENMP
#pragma omp parallel for
#endif
            for (ompIndexType k = 0; k < (ompIndexType)len; k++) {
                double DT = ptd[k];
                double V1, V2, V3, V4, V5, V6, V7;
                DateVector(DT, V1, V2, V3, V4, V5, V6, V7);
                Y[k] = V1;
                M[k] = V2;
                if (nLhs > 2) {
                    D[k] = V3;
                }
                if (nLhs > 3) {
                    H[k] = V4;
                }
                if (nLhs > 4) {
                    MN[k] = V5;
                }
                if (nLhs > 5) {
                    S[k] = V6;
                }
            }
            Dimensions dimParam1 = param1.getDimensions();
            dimParam1.simplify();
            dimParam1.setDimensionLength(0, 1);
            Dimensions dim(param1.getRows(), dimParam1.getElementCount());
            retval << ArrayOf(NLS_DOUBLE, dim, Y);
            retval << ArrayOf(NLS_DOUBLE, dim, M);
            if (nLhs > 2) {
                retval << ArrayOf(NLS_DOUBLE, dim, D);
            }
            if (nLhs > 3) {
                retval << ArrayOf(NLS_DOUBLE, dim, H);
            }
            if (nLhs > 4) {
                retval << ArrayOf(NLS_DOUBLE, dim, MN);
            }
            if (nLhs > 5) {
                retval << ArrayOf(NLS_DOUBLE, dim, S);
            }
        }
    } else {
        OverloadRequired("datevec");
    }
    return retval;
}
//=============================================================================
