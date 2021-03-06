//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// Alternatively, you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of
// the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this program. If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#include "ConvertCharsToStrings.hpp"
#include "Exception.hpp"
#include "IsCellOfStrings.hpp"
#include "nlsConfig.h"
//=============================================================================
namespace Nelson {
ArrayOfVector
ConvertCharsToStrings(const ArrayOfVector& A)
{
    ArrayOfVector res;
    for (size_t q = 0; q < A.size(); ++q) {
        ArrayOf value = A[q];
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
        } else if (IsCellOfString(value)) {
            auto* elementsString = new_with_exception<ArrayOf>(dims.getElementCount(), false);
            ArrayOf valueAsString = ArrayOf(NLS_STRING_ARRAY, dims, elementsString);
            auto* elementsCell = (ArrayOf*)value.getDataPointer();
            ompIndexType elementCount = dims.getElementCount();
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
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
