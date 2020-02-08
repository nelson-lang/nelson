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
#include "IsMissing.hpp"
#include "Error.hpp"
#include "Exception.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
isMissing(const ArrayOf& A, bool& needToOverload)
{
    needToOverload = false;
    Dimensions dimsA = A.getDimensions();
    logical* resultAslogical = nullptr;

    switch (A.getDataClass()) {
    case NLS_STRING_ARRAY: {
        resultAslogical = static_cast<logical*>(
            ArrayOf::allocateArrayOf(NLS_LOGICAL, dimsA.getElementCount(), stringVector(), false));
        auto* elements = (ArrayOf*)A.getDataPointer();
        ompIndexType elementCount = dimsA.getElementCount();
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
        for (ompIndexType k = 0; k < elementCount; k++) {
            resultAslogical[k] = static_cast<Nelson::logical>(!elements[k].isCharacterArray());
        }
    } break;
    case NLS_CELL_ARRAY: {
        resultAslogical = static_cast<logical*>(
            ArrayOf::allocateArrayOf(NLS_LOGICAL, dimsA.getElementCount(), stringVector(), false));
        auto* elements = (ArrayOf*)A.getDataPointer();
        ompIndexType elementCount = dimsA.getElementCount();
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
        for (ompIndexType k = 0; k < elementCount; k++) {
            resultAslogical[k] = static_cast<Nelson::logical>(
                elements[k].isCharacterArray() && elements[k].isEmpty());
        }
    } break;
    case NLS_DOUBLE: {
        resultAslogical = static_cast<logical*>(
            ArrayOf::allocateArrayOf(NLS_LOGICAL, dimsA.getElementCount(), stringVector(), false));
        auto* ptrA = (double*)A.getDataPointer();
        ompIndexType elementCount = dimsA.getElementCount();
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
        for (ompIndexType k = 0; k < elementCount; k++) {
            resultAslogical[k] = static_cast<Nelson::logical>(std::isnan(ptrA[k]));
        }
    } break;
    case NLS_SINGLE: {
        resultAslogical = static_cast<logical*>(
            ArrayOf::allocateArrayOf(NLS_LOGICAL, dimsA.getElementCount(), stringVector(), false));
        auto* ptrA = (single*)A.getDataPointer();
        ompIndexType elementCount = dimsA.getElementCount();
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
        for (ompIndexType k = 0; k < elementCount; k++) {
            resultAslogical[k] = static_cast<Nelson::logical>(std::isnan(ptrA[k]));
        }
    } break;
    case NLS_SCOMPLEX: {
        resultAslogical = static_cast<logical*>(
            ArrayOf::allocateArrayOf(NLS_LOGICAL, dimsA.getElementCount(), stringVector(), false));
        auto* ptrA = (single*)A.getDataPointer();
        ompIndexType elementCount = dimsA.getElementCount() * 2;
        ompIndexType q = 0;
        for (ompIndexType k = 0; k < elementCount; k = k + 2) {
            resultAslogical[q]
                = static_cast<Nelson::logical>(std::isnan(ptrA[k]) || std::isnan(ptrA[k + 1]));
            q++;
        }
    } break;
    case NLS_DCOMPLEX: {
        resultAslogical = static_cast<logical*>(
            ArrayOf::allocateArrayOf(NLS_LOGICAL, dimsA.getElementCount(), stringVector(), false));
        auto* ptrA = (double*)A.getDataPointer();
        ompIndexType elementCount = dimsA.getElementCount() * 2;
        ompIndexType q = 0;
        for (ompIndexType k = 0; k < elementCount; k = k + 2) {
            resultAslogical[q]
                = static_cast<Nelson::logical>(std::isnan(ptrA[k]) || std::isnan(ptrA[k + 1]));
            q++;
        }
    } break;
    case NLS_CHAR: {
        resultAslogical = static_cast<logical*>(
            ArrayOf::allocateArrayOf(NLS_LOGICAL, dimsA.getElementCount(), stringVector(), false));
        auto* ptrA = (charType*)A.getDataPointer();
        ompIndexType elementCount = dimsA.getElementCount();
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
        for (ompIndexType k = 0; k < elementCount; k++) {
            resultAslogical[k] = static_cast<Nelson::logical>(ptrA[k] == L' ');
        }
    } break;
    default: {
        needToOverload = true;
        return ArrayOf();
    } break;
    }
    return ArrayOf(NLS_LOGICAL, dimsA, resultAslogical);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
