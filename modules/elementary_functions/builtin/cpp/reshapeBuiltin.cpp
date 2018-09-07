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
#include "reshapeBuiltin.hpp"
#include "Error.hpp"
#include "OverloadFunction.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::ElementaryFunctionsGateway::reshapeBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (argIn.size() < 2) {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    if (nLhs > 1) {
        Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
    bool bSuccess = false;
    if (eval->mustOverloadBasicTypes()) {
        retval = OverloadFunction(eval, nLhs, argIn, "reshape", bSuccess);
    }
    if (!bSuccess) {
        if (argIn[0].isSparse() || argIn[0].isCell() || argIn[0].isHandle() || argIn[0].isStruct()
            || argIn[0].isClassStruct()) {
            retval = OverloadFunction(eval, nLhs, argIn, "reshape", bSuccess);
            if (bSuccess) {
                return retval;
            }
        }
        ArrayOf M = argIn[0];
        Dimensions dims;
        if (M.isClassStruct()) {
            Error(_W("Undefined function 'reshape' for input arguments of overloaded type."));
        }
        if (M.isFunctionHandle()) {
            Error(
                _W("Undefined function 'reshape' for input arguments of type 'function_handle'."));
        }
        if (argIn.size() == 2 && (!argIn[1].isScalar())) {
            ArrayOf v = argIn[1];
            v.promoteType(NLS_DOUBLE);
            double* dp = (double*)v.getDataPointer();
            for (indexType i = 0; i < v.getLength(); i++) {
                if (!std::isfinite(dp[i])) {
                    Error(_W("finite value expected."));
                }
                if (dp[i] != dp[i]) {
                    Error(_W("finite value expected."));
                }
                int64 ivalue = (int64)dp[i];
                if ((double)ivalue != dp[i]) {
                    Error(_W("real integer expected."));
                }
                if (ivalue < 0) {
                    Error(_W("real positive integer expected."));
                }
                dims[i] = (indexType)ivalue;
            }
        } else {
            if (argIn.size() < 3) {
                Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
            }
            bool bAllIsScalar = true;
            for (size_t j = 1; j < argIn.size(); j++) {
                bAllIsScalar &= argIn[j].isScalar();
            }
            if (bAllIsScalar) {
                for (size_t i = 1; i < argIn.size(); i++) {
                    ArrayOf arg = argIn[i];
                    indexType idx = arg.getContentAsScalarIndex();
                    dims[i - 1] = idx;
                }
            } else {
                indexType idxEmptyPosition;
                bool bHaveAnEmptyMatrix = false;
                for (size_t i = 1; i < argIn.size(); i++) {
                    if (bHaveAnEmptyMatrix) {
                        if (argIn[i].isEmpty()) {
                            Error(_W("only one unknown dimension allowed."));
                        }
                    } else {
                        if (argIn[i].isEmpty()) {
                            bHaveAnEmptyMatrix = argIn[i].isEmpty();
                            idxEmptyPosition = (indexType)i - 1;
                        }
                    }
                }
                for (size_t i = 1; i < argIn.size(); i++) {
                    ArrayOf p = argIn[i];
                    if (p.isEmpty()) {
                        dims[i - 1] = 1;
                    } else {
                        indexType idx = p.getContentAsScalarIndex();
                        dims[i - 1] = idx;
                    }
                }
                if (bHaveAnEmptyMatrix) {
                    indexType nbElements = 0;
                    indexType nbElementsM = M.getDimensions().getElementCount();
                    double rest = (double)nbElementsM / (double)dims.getElementCount();
                    if (!std::isfinite(rest)) {
                        Error(_W("finite value expected."));
                    }
                    if (rest != rest) {
                        Error(_W("finite value expected."));
                    }
                    int64 ivalue = (int64)rest;
                    if ((double)ivalue != rest) {
                        Error(_W("real integer expected."));
                    }
                    if (ivalue < 0) {
                        Error(_W("real positive integer expected."));
                    }
                    dims[idxEmptyPosition] = (indexType)ivalue;
                }
            }
        }
        if (dims.getElementCount() != M.getLength()) {
            Error(_W("Reshape operation cannot change the number of elements in array."));
        }
        M.reshape(dims);
        retval.push_back(M);
    }
    return retval;
}
//=============================================================================
