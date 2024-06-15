//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "StringLength.hpp"
#include "Error.hpp"
#include "nlsBuildConfig.h"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
StringLength(const ArrayOf& A)
{
    ArrayOf res;
    Dimensions outputDims;
    double* ptrLength = nullptr;
    if (A.isStringArray()) {
        outputDims = A.getDimensions();
        auto* elements = (ArrayOf*)A.getDataPointer();
        ptrLength = static_cast<double*>(ArrayOf::allocateArrayOf(
            NLS_DOUBLE, outputDims.getElementCount(), stringVector(), false));
        ompIndexType elementCount = outputDims.getElementCount();
#if WITH_OPENMP
#pragma omp parallel for
#endif
        for (ompIndexType k = 0; k < elementCount; k++) {
            if (elements[k].isCharacterArray()) {
                std::wstring wstr = elements[k].getContentAsWideString();
                ptrLength[k] = static_cast<double>(wstr.length());
            } else {
                ptrLength[k] = std::nan("NaN");
            }
        }
    } else {
        wstringVector wstr = A.getContentAsWideStringVector(false);
        if (A.isCharacterArray() && wstr.empty()) {
            wstr.push_back(A.getContentAsWideString());
        }
        if (wstr.size() == 1) {
            outputDims = Dimensions(1, 1);
        } else {
            outputDims = A.getDimensions();
        }
        ptrLength = static_cast<double*>(ArrayOf::allocateArrayOf(
            NLS_DOUBLE, outputDims.getElementCount(), stringVector(), false));
        ompIndexType s = (ompIndexType)wstr.size();
#if WITH_OPENMP
#pragma omp parallel for
#endif
        for (ompIndexType k = 0; k < s; k++) {
            ptrLength[k] = static_cast<double>(wstr[k].length());
        }
    }
    return ArrayOf(NLS_DOUBLE, outputDims, ptrLength);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
