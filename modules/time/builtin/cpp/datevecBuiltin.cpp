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
#include <string>
#include "datevecBuiltin.hpp"
#include "Error.hpp"
#include "DateVector.hpp"
#include "OverloadFunction.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::TimeGateway::datevecBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (argIn.size() == 0) {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    if (nLhs > 6) {
        Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
    bool bSuccess = false;
    if (eval->mustOverloadBasicTypes()) {
        retval = OverloadFunction(eval, nLhs, argIn, "datevec", bSuccess);
    }
    if (!bSuccess) {
        if (argIn.size() != 1) {
            Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
        }
        ArrayOf param1 = argIn[0];
        if (((param1.getDataClass() == NLS_DOUBLE) || (param1.getDataClass() == NLS_DCOMPLEX))
            && (!param1.isSparse())) {
            if (param1.getDataClass() == NLS_DCOMPLEX) {
                param1.promoteType(NLS_DOUBLE);
            }
            indexType len = param1.getLength();
            double* ptd = (double*)param1.getDataPointer();
            if (nLhs < 2) {
                double* res = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, 6 * len);
                for (indexType k = 0; k < len; k++) {
                    double DT, Y, M, D, H, MN, S;
                    DT = ptd[k];
                    DateVector(DT, Y, M, D, H, MN, S);
                    res[k] = Y;
                    res[k + len] = M;
                    res[k + len * 2] = D;
                    res[k + len * 3] = H;
                    res[k + len * 4] = MN;
                    res[k + len * 5] = S;
                }
                Dimensions dim(len, 6);
                retval.push_back(ArrayOf(NLS_DOUBLE, dim, res));
            } else {
                double* Y = nullptr;
                double* M = nullptr;
                double* D = nullptr;
                double* H = nullptr;
                double* MN = nullptr;
                double* S = nullptr;

                Y = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, len);
                M = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, len);
                if (nLhs > 2) {
                    D = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, len);
                }
                if (nLhs > 3) {
                    H = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, len);
                }
                if (nLhs > 4) {
                    MN = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, len);
                }
                if (nLhs > 5) {
                    S = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, len);
                }
                for (indexType k = 0; k < len; k++) {
                    double DT = ptd[k];
                    double V1, V2, V3, V4, V5, V6;
                    DateVector(DT, V1, V2, V3, V4, V5, V6);
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
                Dimensions dim(param1.getDimensions().getRows(), dimParam1.getElementCount());
                retval.push_back(ArrayOf(NLS_DOUBLE, dim, Y));
                retval.push_back(ArrayOf(NLS_DOUBLE, dim, M));
                if (nLhs > 2) {
                    retval.push_back(ArrayOf(NLS_DOUBLE, dim, D));
                }
                if (nLhs > 3) {
                    retval.push_back(ArrayOf(NLS_DOUBLE, dim, H));
                }
                if (nLhs > 4) {
                    retval.push_back(ArrayOf(NLS_DOUBLE, dim, MN));
                }
                if (nLhs > 5) {
                    retval.push_back(ArrayOf(NLS_DOUBLE, dim, S));
                }
            }
        } else {
            retval = OverloadFunction(eval, nLhs, argIn, "datevec");
        }
    }
    return retval;
}
//=============================================================================
