//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "StringDeblank.hpp"
#include "StringTrim.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "nlsBuildConfig.h"
#include "omp_for_loop.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
// deblank removes trailing whitespace and NUL characters; non-text values
// (numbers, nested cells, ...) are returned unchanged.
ArrayOf
StringDeblank(const ArrayOf& A, bool& needToOverload)
{
    needToOverload = false;
    if (A.isCharacterArray()) {
        if (A.isEmpty()) {
            return ArrayOf::characterArray(L"");
        }
        return TrimCharacterArray(A, false, true);
    }
    if (A.isCell()) {
        if (A.isEmpty()) {
            return A;
        }
        ArrayOf res(A);
        res.ensureSingleOwner();
        auto* element = (ArrayOf*)res.dataPointer();
        indexType elementCount = A.elementCount();
        for (indexType k = 0; k < elementCount; k++) {
            if (element[k].isCharacterArray() || element[k].isStringArray()) {
                bool over = false;
                element[k] = StringDeblank(element[k], over);
            }
        }
        return res;
    }
    if (A.isStringArray()) {
        if (A.isEmpty()) {
            return A;
        }
        ArrayOf res(A);
        res.ensureSingleOwner();
        auto* element = (ArrayOf*)res.dataPointer();
        ompIndexType elementCount = A.elementCount();
        OMP_PARALLEL_FOR_LOOP(elementCount)
        for (ompIndexType k = 0; k < elementCount; k++) {
            if (element[k].isCharacterArray()) {
                element[k] = TrimCharacterArray(element[k], false, true);
            }
        }
        return res;
    }
    if (A.isNumeric() || A.isLogical()) {
        return A;
    }
    needToOverload = true;
    return {};
}
//=============================================================================
} // namespace Nelson
//=============================================================================
