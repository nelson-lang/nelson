//=============================================================================
// Copyright (c) 2016-2018 Allan CORNET (Nelson)
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
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
        resultAslogical = (logical*)ArrayOf::allocateArrayOf(
            NLS_LOGICAL, dimsA.getElementCount(), stringVector(), false);
        ArrayOf* elements = (ArrayOf*)A.getDataPointer();
        for (indexType k = 0; k < dimsA.getElementCount(); k++) {
            resultAslogical[k] = !elements[k].isCharacterArray();
        }
    } break;
    case NLS_CELL_ARRAY: {
        resultAslogical = (logical*)ArrayOf::allocateArrayOf(
            NLS_LOGICAL, dimsA.getElementCount(), stringVector(), false);
        ArrayOf* elements = (ArrayOf*)A.getDataPointer();
        for (indexType k = 0; k < dimsA.getElementCount(); k++) {
            resultAslogical[k] = elements[k].isCharacterArray() && elements[k].isEmpty();
        }
    } break;
    case NLS_DOUBLE: {
        resultAslogical = (logical*)ArrayOf::allocateArrayOf(
            NLS_LOGICAL, dimsA.getElementCount(), stringVector(), false);
        double* ptrA = (double*)A.getDataPointer();
        for (indexType k = 0; k < dimsA.getElementCount(); k++) {
            resultAslogical[k] = std::isnan(ptrA[k]);
        }
    } break;
    case NLS_SINGLE: {
        resultAslogical = (logical*)ArrayOf::allocateArrayOf(
            NLS_LOGICAL, dimsA.getElementCount(), stringVector(), false);
        single* ptrA = (single*)A.getDataPointer();
        for (indexType k = 0; k < dimsA.getElementCount(); k++) {
            resultAslogical[k] = std::isnan(ptrA[k]);
        }
    } break;
    case NLS_SCOMPLEX: {
        resultAslogical = (logical*)ArrayOf::allocateArrayOf(
            NLS_LOGICAL, dimsA.getElementCount(), stringVector(), false);
        single* ptrA = (single*)A.getDataPointer();
        for (indexType k = 0; k < dimsA.getElementCount() * 2; k++) {
            resultAslogical[k] = std::isnan(ptrA[k]) || std::isnan(ptrA[k + 1]);
        }
    } break;
    case NLS_DCOMPLEX: {
        resultAslogical = (logical*)ArrayOf::allocateArrayOf(
            NLS_LOGICAL, dimsA.getElementCount(), stringVector(), false);
        double* ptrA = (double*)A.getDataPointer();
        for (indexType k = 0; k < dimsA.getElementCount() * 2; k++) {
            resultAslogical[k] = std::isnan(ptrA[k]) || std::isnan(ptrA[k + 1]);
        }
    } break;
    case NLS_CHAR: {
        resultAslogical = (logical*)ArrayOf::allocateArrayOf(
            NLS_LOGICAL, dimsA.getElementCount(), stringVector(), false);
        charType* ptrA = (charType*)A.getDataPointer();
        for (indexType k = 0; k < dimsA.getElementCount(); k++) {
            resultAslogical[k] = (ptrA[k] == L' ');
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
}
//=============================================================================
