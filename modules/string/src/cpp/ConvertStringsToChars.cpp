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
        Dimensions dims = A.getDimensions();
        if (dims.isEmpty(false)) {
            res = ArrayOf(NLS_CELL_ARRAY, dims, nullptr);
        } else if (dims.isScalar()) {
            auto* elementsStr = (ArrayOf*)A.getDataPointer();
            ArrayOf element = elementsStr[0];
            if (element.getDataClass() == NLS_CHAR) {
                res = ArrayOf::characterArrayConstructor(element.getContentAsWideString());
            } else {
                if (missingAsNaN) {
                    auto* elementsCell = new_with_exception<ArrayOf>(dims.getElementCount(), false);
                    res = ArrayOf(NLS_CELL_ARRAY, dims, elementsCell);
                    elementsCell[0] = ArrayOf::doubleConstructor(std::nan(""));
                } else {
                    res = ArrayOf::characterArrayConstructor("");
                }
            }
        } else {
            auto* elementsCell = new_with_exception<ArrayOf>(dims.getElementCount(), false);
            ArrayOf valueAsCell = ArrayOf(NLS_CELL_ARRAY, dims, elementsCell);
            auto* elementsStr = (ArrayOf*)A.getDataPointer();
            ompIndexType elementCount = dims.getElementCount();
            OMP_PARALLEL_FOR_LOOP(elementCount)
            for (ompIndexType q = 0; q < elementCount; q++) {
                if (elementsStr[q].getDataClass() == NLS_CHAR) {
                    elementsCell[q] = elementsStr[q];
                } else {
                    if (missingAsNaN) {
                        elementsCell[q] = ArrayOf::doubleConstructor(std::nan(""));
                    } else {
                        elementsCell[q] = ArrayOf::characterArrayConstructor("");
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
