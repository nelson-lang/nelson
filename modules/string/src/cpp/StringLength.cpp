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
#include "StringLength.hpp"
#include "Error.hpp"
#include "nlsConfig.h"
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
#if defined(_NLS_WITH_OPENMP)
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
#if defined(_NLS_WITH_OPENMP)
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
