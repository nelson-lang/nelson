//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "ConvertStringsToChars.hpp"
#include "NewWithException.hpp"
#include "nlsBuildConfig.h"
#include "omp_for_loop.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
ConvertStringsToChars(const ArrayOf& A, bool missingAsNaN)
{
    ArrayOf res;
    if (A.isStringArray()) {
        Dimensions dims = A.dimensions();
        if (dims.isEmpty(false)) {
            res = ArrayOf(NLS_CELL_ARRAY, dims, nullptr);
        } else if (dims.isScalar()) {
            auto* elementsStr = (ArrayOf*)A.dataPointer();
            ArrayOf element = elementsStr[0];
            if (element.dataClass() == NLS_CHAR) {
                res = ArrayOf::characterArray(element.asWideString());
            } else {
                if (missingAsNaN) {
                    auto* elementsCell = new_with_exception<ArrayOf>(dims.elementCount(), false);
                    res = ArrayOf(NLS_CELL_ARRAY, dims, elementsCell);
                    elementsCell[0] = ArrayOf::doubleScalar(std::nan(""));
                } else {
                    res = ArrayOf::characterArray("");
                }
            }
        } else {
            auto* elementsCell = new_with_exception<ArrayOf>(dims.elementCount(), false);
            ArrayOf valueAsCell = ArrayOf(NLS_CELL_ARRAY, dims, elementsCell);
            auto* elementsStr = (ArrayOf*)A.dataPointer();
            ompIndexType elementCount = dims.elementCount();
            OMP_PARALLEL_FOR_LOOP(elementCount)
            for (ompIndexType q = 0; q < elementCount; q++) {
                if (elementsStr[q].dataClass() == NLS_CHAR) {
                    elementsCell[q] = elementsStr[q];
                } else {
                    if (missingAsNaN) {
                        elementsCell[q] = ArrayOf::doubleScalar(std::nan(""));
                    } else {
                        elementsCell[q] = ArrayOf::characterArray("");
                    }
                }
            }
            res = valueAsCell;
        }
    } else {
        res = A;
    }
    return res;
}
//=============================================================================
ArrayOfVector
ConvertStringsToChars(const ArrayOfVector& A)
{
    ArrayOfVector res;
    for (const auto& value : A) {
        res.push_back(ConvertStringsToChars(value, false));
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
