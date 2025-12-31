//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <memory.h>
#include "Permute.hpp"
#include "Transpose.hpp"
#include "nlsBuildConfig.h"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
Permute(
    const ArrayOf& arrayIn, const std::vector<indexType>& permutationVector, bool& needToOverload)
{
    ArrayOf res;
    bool isSupportedTypes = (arrayIn.getDataClass() <= NLS_CHAR) && !arrayIn.isSparse()
        || arrayIn.isStringArray() || arrayIn.isCell() || (arrayIn.isStruct());
    if (!isSupportedTypes) {
        needToOverload = true;
        return arrayIn;
    }
    Dimensions dimsIn = arrayIn.getDimensions();

    indexType Adims = dimsIn.getLength();
    bool id_perm = true;
    for (indexType i = 0; i < Adims; i++) {
        if ((permutationVector[i] - 1) != (indexType)i) {
            id_perm = false;
        }
    }
    if (id_perm) {
        return arrayIn;
    }

    if (arrayIn.isEmpty()) {
        if (dimsIn.getLength() < permutationVector.size()) {
            for (indexType k = dimsIn.getLength(); k < (indexType)permutationVector.size(); k++) {
                dimsIn.setDimensionLength(k, 1);
            }
        }
        Dimensions newdims(dimsIn.permute(permutationVector, true));
        return ArrayOf::emptyConstructor(newdims, arrayIn.isSparse());
    }

    if (arrayIn.isScalar()) {
        return arrayIn;
    }

    if (arrayIn.is2D()) {
        bool isTranspose = true;
        for (int i = 2; i < dimsIn.getLength(); i++) {
            if (permutationVector[i] != (i + 1)) {
                isTranspose = false;
            }
        }
        if (permutationVector[0] != 2) {
            isTranspose = false;
        }
        if (permutationVector[1] != 1) {
            isTranspose = false;
        }
        if (isTranspose) {
            return Transpose(arrayIn, needToOverload);
        }
        res = arrayIn;
        Dimensions newdims(dimsIn.permute(permutationVector, false));
        res.reshape(newdims);
        return res;
    }
    Dimensions newdims(dimsIn.permute(permutationVector, false));

    char* sp = (char*)arrayIn.getDataPointer();
    void* dst_data = ArrayOf::allocateArrayOf(
        arrayIn.getDataClass(), newdims.getElementCount(), arrayIn.getFieldNames(), true);

    newdims.simplify();
    res = ArrayOf(
        arrayIn.getDataClass(), newdims, dst_data, arrayIn.isSparse(), arrayIn.getFieldNames());
    Dimensions curPos(dimsIn.getLength());
    indexType elSize(arrayIn.getElementSize());
    char* qp = (char*)dst_data;
    for (indexType srcIndex = 0; srcIndex < newdims.getElementCount(); srcIndex++) {
        indexType dstIndex = newdims.mapPoint(curPos.permute(permutationVector, true));
        if (arrayIn.isCell() || arrayIn.isStringArray() || arrayIn.isStruct()) {
            const ArrayOf* pSrc = (const ArrayOf*)arrayIn.getDataPointer();
            ArrayOf* pDest = (ArrayOf*)dst_data;
            pDest[dstIndex] = pSrc[srcIndex];
        } else {
            ::memcpy(qp + dstIndex * elSize, sp + srcIndex * elSize, 1 * elSize);
        }
        curPos.incrementModulo(dimsIn, 0);
    }
    return res;
}
//=============================================================================
}
//=============================================================================
