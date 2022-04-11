//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
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
    // R = repmat(A, m, n, p �)
    // R = repmat(A, [m n])
    // R = repmat(A, [m n p �])
    ArrayOfVector retval;
    nargincheck(argIn, 2);
    nargoutcheck(nLhs, 0, 1);
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
        NelsonType classx = x.getDataClass();
        bool isNotSupportedType = (classx == NLS_HANDLE || classx == NLS_GO_HANDLE || x.isSparse());
        if (isNotSupportedType) {
            Error(ERROR_TYPE_NOT_SUPPORTED);
        }
        switch (argIn.size()) {
        case 2: {
            ArrayOf param2 = argIn[1];
            if (param2.isScalar()) {
                repcount[0] = (indexType)param2.getContentAsUnsignedInteger64Scalar();
                repcount[1] = (indexType)param2.getContentAsUnsignedInteger64Scalar();
            } else {
                if (param2.isRowVector()) {
                    param2.promoteType(NLS_UINT64);
                    if (param2.getElementCount() > maxDims) {
                        Error(_W("Too many dimensions!"));
                    }
                    auto* dp = (uint64*)param2.getDataPointer();
                    for (indexType i = 0; i < param2.getElementCount(); i++) {
                        repcount[i] = (indexType)dp[i];
                    }
                } else {
                    Error(_W("An row vector expected."));
                }
            }
        } break;
        default: {
            for (size_t k = 1; k < argIn.size(); ++k) {
                ArrayOf paramK = argIn[k];
                repcount[k - 1] = (indexType)paramK.getContentAsUnsignedInteger64Scalar();
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
        void* dp
            = ArrayOf::allocateArrayOf(classx, outdims.getElementCount(), x.getFieldNames(), true);
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
        retval << res;
    }
    return retval;
}
//=============================================================================
