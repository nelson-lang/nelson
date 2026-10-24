//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <cmath>
#include "StringLength.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "nlsBuildConfig.h"
#include "omp_for_loop.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static bool
isCharacterVector(const ArrayOf& A);
static void
invalidInputError();
//=============================================================================
ArrayOf
StringLength(const ArrayOf& A)
{
    Dimensions outputDims = A.dimensions();
    if (A.isCharacterArray()) {
        if (!isCharacterVector(A)) {
            invalidInputError();
        }
        outputDims = Dimensions(1, 1);
    } else if (!A.isStringArray() && !A.isCell()) {
        invalidInputError();
    }
    auto* ptrLength = static_cast<double*>(
        ArrayOf::allocateArrayOf(NLS_DOUBLE, outputDims.elementCount(), stringVector(), false));
    ArrayOf res(NLS_DOUBLE, outputDims, ptrLength);
    if (A.isCharacterArray()) {
        ptrLength[0] = static_cast<double>(A.elementCount());
        return res;
    }
    auto* elements = (ArrayOf*)A.dataPointer();
    ompIndexType elementCount = outputDims.elementCount();
    if (A.isCell()) {
        for (ompIndexType k = 0; k < elementCount; k++) {
            if (!isCharacterVector(elements[k])) {
                invalidInputError();
            }
        }
    }
    OMP_PARALLEL_FOR_LOOP(elementCount)
    for (ompIndexType k = 0; k < elementCount; k++) {
        if (elements[k].isCharacterArray()) {
            ptrLength[k] = static_cast<double>(elements[k].elementCount());
        } else {
            ptrLength[k] = std::nan("");
        }
    }
    return res;
}
//=============================================================================
// A character vector is a row of characters or the empty text ''.
static bool
isCharacterVector(const ArrayOf& A)
{
    return A.isCharacterArray() && !A.isSparse() && (A.isRowVector() || A.isEmpty(true));
}
//=============================================================================
static void
invalidInputError()
{
    Error(L"Nelson:string:strlength:invalidInput",
        L"First argument must be a character vector, a string array or a cell array of "
        L"character vectors.");
}
//=============================================================================
} // namespace Nelson
//=============================================================================
