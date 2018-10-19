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
#include "IJVBuiltin.hpp"
#include "Error.hpp"
#include "OverloadFunction.hpp"
#include "SparseType.hpp"
#include "SparseNonZeros.hpp"
#include "SparseToIJV.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::SparseGateway::IJVBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (nLhs > 6) {
        Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
    if (argIn.size() != 1) {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
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
            retval.push_back(I);
            if (nLhs > 1) {
                retval.push_back(J);
            }
            if (nLhs > 2) {
                retval.push_back(V);
            }
            if (nLhs > 3) {
                retval.push_back(M);
            }
            if (nLhs > 4) {
                retval.push_back(N);
            }
            if (nLhs > 5) {
                retval.push_back(NNZ);
            }
        }
    }
    return retval;
}
//=============================================================================
