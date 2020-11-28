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
#include "ConvertStringsToChars.hpp"
#include "Exception.hpp"
#include "nlsConfig.h"
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
                    elementsCell[0] = ArrayOf::doubleConstructor(std::nan("NaN"));
                } else {
                    res = ArrayOf::characterArrayConstructor("");
                }
            }
        } else {
            auto* elementsCell = new_with_exception<ArrayOf>(dims.getElementCount(), false);
            ArrayOf valueAsCell = ArrayOf(NLS_CELL_ARRAY, dims, elementsCell);
            auto* elementsStr = (ArrayOf*)A.getDataPointer();
            ompIndexType elementCount = dims.getElementCount();
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
            for (ompIndexType q = 0; q < elementCount; q++) {
                if (elementsStr[q].getDataClass() == NLS_CHAR) {
                    elementsCell[q] = elementsStr[q];
                } else {
                    if (missingAsNaN) {
                        elementsCell[q] = ArrayOf::doubleConstructor(std::nan("NaN"));
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
    for (auto value : A) {
        res.push_back(ConvertStringsToChars(value, false));
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
