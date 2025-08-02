//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "ConvertCharsToStrings.hpp"
#include "NewWithException.hpp"
#include "nlsBuildConfig.h"
#include "omp_for_loop.hpp"
//=============================================================================
namespace Nelson {
ArrayOfVector
ConvertCharsToStrings(const ArrayOfVector& A)
{
    ArrayOfVector res;
    for (const auto& value : A) {
        Dimensions dims = value.getDimensions();
        if (value.isCharacterArray()) {
            if (value.isEmpty(false)) {
                res.push_back(ArrayOf::stringArrayConstructor(""));
            } else if (value.isColumnVector()) {
                res.push_back(ArrayOf::stringArrayConstructor(value.getContentAsWideString()));
            } else {
                indexType len = dims.getElementCount() / dims.getRows();
                auto* ptrChar = (charType*)value.getDataPointer();
                std::wstring str;
                str.reserve(dims.getElementCount());
                for (indexType i = 0; i < dims.getRows(); i++) {
                    for (indexType j = 0; j < len; j++) {
                        size_t idx = i + j * dims.getRows();
                        if (ptrChar[idx] != 0) {
                            str.push_back(ptrChar[idx]);
                        }
                    }
                }
                res.push_back(ArrayOf::stringArrayConstructor(str));
            }
        } else if (value.isCellArrayOfCharacterVectors()) {
            auto* elementsString = new_with_exception<ArrayOf>(dims.getElementCount(), false);
            ArrayOf valueAsString = ArrayOf(NLS_STRING_ARRAY, dims, elementsString);
            auto* elementsCell = (ArrayOf*)value.getDataPointer();
            ompIndexType elementCount = dims.getElementCount();
            OMP_PARALLEL_FOR_LOOP(elementCount)
            for (ompIndexType q = 0; q < elementCount; q++) {
                elementsString[q] = elementsCell[q];
            }
            res.push_back(valueAsString);
        } else {
            res.push_back(value);
        }
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
