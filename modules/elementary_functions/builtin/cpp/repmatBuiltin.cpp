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
#include "repmatBuiltin.hpp"
#include "Error.hpp"
#include "OverloadFunction.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::ElementaryFunctionsGateway::repmatBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    // R = repmat(A, m)
    // R = repmat(A, m, n)
    // R = repmat(A, m, n, p …)
    // R = repmat(A, [m n])
    // R = repmat(A, [m n p …])
    ArrayOfVector retval;
    if (argIn.size() < 2) {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    if (nLhs > 1) {
        Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
    bool bSuccess = false;
    if (eval->mustOverloadBasicTypes()) {
        retval = OverloadFunction(eval, nLhs, argIn, "repmat", bSuccess);
    }
    if (!bSuccess) {
        if (argIn[0].isSparse() || argIn[0].isCell() || argIn[0].isHandle() || argIn[0].isStruct()
            || argIn[0].isClassStruct()) {
            retval = OverloadFunction(eval, nLhs, argIn, "repmat", bSuccess);
            if (bSuccess) {
                return retval;
            }
        }
        Dimensions repcount;
        ArrayOf x = argIn[0];
        Class classx = x.getDataClass();
        bool isNotSupportedType = (classx == NLS_HANDLE || x.isSparse());
        if (isNotSupportedType) {
            Error(ERROR_TYPE_NOT_SUPPORTED);
        }
        switch (argIn.size()) {
        case 2: {
            ArrayOf param2 = argIn[1];
            if (param2.isScalar()) {
                repcount[0] = param2.getContentAsUnsignedInt64Scalar();
                repcount[1] = param2.getContentAsUnsignedInt64Scalar();
            } else {
                if (param2.isRowVector()) {
                    param2.promoteType(NLS_UINT64);
                    if (param2.getLength() > maxDims) {
                        Error(_W("Too many dimensions!"));
                    }
                    uint64* dp = (uint64*)param2.getDataPointer();
                    for (indexType i = 0; i < param2.getLength(); i++) {
                        repcount[i] = dp[i];
                    }
                } else {
                    Error(_W("An row vector expected."));
                }
            }
        } break;
        default: {
            for (size_t k = 1; k < argIn.size(); ++k) {
                ArrayOf paramK = argIn[k];
                repcount[k - 1] = paramK.getContentAsUnsignedInt64Scalar();
            }
        } break;
        }
        Dimensions originalSize(x.getDimensions());
        indexType outdim = (repcount.getLength()) > (originalSize.getLength())
            ? (repcount.getLength())
            : (originalSize.getLength());
        Dimensions outdims;
        for (indexType i = 0; i < outdim; i++) {
            outdims[i] = originalSize[i] * repcount[i];
        }
        outdims.simplify();
        void* dp = ArrayOf::allocateArrayOf(classx, outdims.getElementCount(), x.getFieldNames());
        indexType colsize = originalSize[0];
        indexType outcolsize = outdims[0];
        indexType colcount = originalSize.getElementCount() / colsize;
        Dimensions copySelection(outdim);
        Dimensions sourceDimensions(originalSize.getLength());
        indexType copyCount = repcount.getElementCount();
        Dimensions anchor(outdim);
        for (indexType i = 0; i < copyCount; i++) {
            sourceDimensions.zeroOut();
            for (indexType j = 0; j < colcount; j++) {
                for (indexType k = 0; k < outdim; k++) {
                    anchor[k] = copySelection[k] * originalSize[k] + sourceDimensions[k];
                }
                x.copyElements(j * colsize, dp, outdims.mapPoint(anchor), colsize);
                sourceDimensions.incrementModulo(originalSize, 1);
            }
            copySelection.incrementModulo(repcount, 0);
        }
        ArrayOf res;
        if (classx == NLS_STRUCT_ARRAY) {
            res = ArrayOf(classx, outdims, dp, false, x.getFieldNames());
        } else {
            res = ArrayOf(classx, outdims, dp);
        }
        retval.push_back(res);
    }
    return retval;
}
//=============================================================================
