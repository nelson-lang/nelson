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
#include "nlsConfig.h"
#include "IsMember.hpp"
#include "Error.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static bool
isCellOfString(const ArrayOf& A)
{
    if (A.isCell()) {
        indexType nbElements = A.getElementCount();
        ArrayOf* elements = (ArrayOf*)A.getDataPointer();
        for (indexType k = 0; k < nbElements; ++k) {
            if (!elements[k].isCharacterArray()) {
                return false;
            }
        }
        return true;
    }
    return false;
}
//=============================================================================
static ArrayOf
isMemberStringArray(const ArrayOf& A, const ArrayOf& B)
{
    logical* elements = (logical*)ArrayOf::allocateArrayOf(NLS_LOGICAL, A.getElementCount());
    ArrayOf res = ArrayOf(NLS_LOGICAL, A.getDimensions(), elements);
    memset(elements, false, sizeof(logical) * A.getElementCount());
    ArrayOf* dA = (ArrayOf*)A.getDataPointer();
    ArrayOf* dB = (ArrayOf*)B.getDataPointer();
    bool isMissingA = false;
    ompIndexType nbElementsA = A.getElementCount();
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
    for (ompIndexType k = 0; k < nbElementsA; ++k) {
        std::wstring strA;
        if (dA[k].getDataClass() != NLS_CHAR) {
            isMissingA = true;
        } else {
            strA = dA[k].getContentAsWideString();
        }
        for (indexType q = 0; q < B.getElementCount(); ++q) {
            bool isMissingB = dB[k].getDataClass() != NLS_CHAR;
            if (!isMissingA && !isMissingB) {
                std::wstring strB = dB[k].getContentAsWideString();
                if (strA == strB) {
                    elements[k] = true;
                    break;
                }
            }
        }
    }
}
//=============================================================================
static ArrayOf
isMemberCharacterArray(const ArrayOf& A, const ArrayOf& B)
{
    ArrayOf res;
    wstringVector stringsA = A.getContentAsWideStringVector(false);
    wstringVector stringsB = B.getContentAsWideStringVector(false);
    Dimensions dimsRes(1, stringsA.size());
    if (stringsA.size() == 0) {
        dimsRes.setDimensionLength(0, 0);
    }
    logical* elements = (logical*)ArrayOf::allocateArrayOf(NLS_LOGICAL, stringsA.size());
    res = ArrayOf(NLS_LOGICAL, dimsRes, elements);
    memset(elements, false, sizeof(logical) * stringsA.size());
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
    for (ompIndexType k = 0; k < (ompIndexType)stringsA.size(); ++k) {
        std::wstring value = stringsA[k];
        for (indexType q = 0; q < stringsB.size(); ++q) {
            if (value == stringsB[q]) {
                elements[k] = true;
                break;
            }
        }
    }
    return res;
}
//=============================================================================
template <class T>
ArrayOf
isMemberReal(const ArrayOf& A, const ArrayOf& B)
{
    logical* elements = (logical*)ArrayOf::allocateArrayOf(NLS_LOGICAL, A.getElementCount());
    ArrayOf res = ArrayOf(NLS_LOGICAL, A.getDimensions(), elements);
    memset(elements, false, sizeof(logical) * A.getElementCount());
    T* dA = (T*)A.getDataPointer();
    T* dB = (T*)B.getDataPointer();
    ompIndexType nbElementsA = A.getElementCount();
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
    for (ompIndexType k = 0; k < nbElementsA; ++k) {
        T value = dA[k];
        for (indexType q = 0; q < B.getElementCount(); ++q) {
            if (value == dB[q]) {
                elements[k] = true;
                break;
            }
        }
    }
    return res;
}
//=============================================================================
template <class T>
ArrayOf
isMemberComplex(const ArrayOf& A, const ArrayOf& B)
{
    logical* elements = (logical*)ArrayOf::allocateArrayOf(NLS_LOGICAL, A.getElementCount());
    ArrayOf res = ArrayOf(NLS_LOGICAL, A.getDimensions(), elements);
    memset(elements, false, sizeof(logical) * A.getElementCount());
    T* dA = (T*)A.getDataPointer();
    T* dB = (T*)B.getDataPointer();
    ompIndexType nbElementsA = A.getElementCount();
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
    for (ompIndexType k = 0; k < nbElementsA * 2; k = k + 2) {
        std::complex<T> value(dA[k], dA[k + 1]);
        for (indexType q = 0; q < B.getElementCount() * 2; q = q + 2) {
            if (value == std::complex<T>(dB[q], dB[q + 1])) {
                elements[k / 2] = true;
                break;
            }
        }
    }
    return res;
}
//=============================================================================
ArrayOf
IsMember(const ArrayOf& A, const ArrayOf& B, bool& needToOverload)
{
    ArrayOf res;
    needToOverload = false;
    bool isTextA = A.isCharacterArray() || A.isStringArray() || isCellOfString(A);
    bool isTextB = B.isCharacterArray() || B.isStringArray() || isCellOfString(B);
    if (A.isCharacterArray() && B.isCharacterArray()) {
        res = isMemberReal<charType>(A, B);
    } else if (isTextA && isTextB) {
        res = isMemberCharacterArray(A, B);
    } else if (A.getDataClass() == B.getDataClass()) {
        switch (A.getDataClass()) {
        case NLS_STRING_ARRAY: {
            res = isMemberStringArray(A, B);
        } break;
        case NLS_LOGICAL: {
            res = isMemberReal<logical>(A, B);
        } break;
        case NLS_UINT8: {
            res = isMemberReal<uint8>(A, B);
        } break;
        case NLS_INT8: {
            res = isMemberReal<int8>(A, B);
        } break;
        case NLS_UINT16: {
            res = isMemberReal<uint16>(A, B);
        } break;
        case NLS_INT16: {
            res = isMemberReal<int16>(A, B);
        } break;
        case NLS_UINT32: {
            res = isMemberReal<uint32>(A, B);
        } break;
        case NLS_INT32: {
            res = isMemberReal<int32>(A, B);
        } break;
        case NLS_UINT64: {
            res = isMemberReal<uint64>(A, B);
        } break;
        case NLS_INT64: {
            res = isMemberReal<int64>(A, B);
        } break;
        case NLS_SINGLE: {
            res = isMemberReal<single>(A, B);
        } break;
        case NLS_DOUBLE: {
            res = isMemberReal<double>(A, B);
        } break;
        case NLS_SCOMPLEX: {
            res = isMemberComplex<single>(A, B);
        } break;
        case NLS_DCOMPLEX: {
            res = isMemberComplex<double>(A, B);
        } break;
        case NLS_CHAR: {
            res = isMemberComplex<charType>(A, B);
        } break;
        default: {
            needToOverload = true;
        } break;
        }
    } else {
        if (A.isDoubleClass() || B.isDoubleClass()) {
            ArrayOf AA(A);
            ArrayOf BB(B);
            if (!AA.canBePromotedTo(NLS_DCOMPLEX)) {
                needToOverload = true;
                return res;
            }
            if (!BB.canBePromotedTo(NLS_DCOMPLEX)) {
                needToOverload = true;
                return res;
            }
            AA.promoteType(NLS_DCOMPLEX);
            BB.promoteType(NLS_DCOMPLEX);
            res = IsMember(AA, BB, needToOverload);
        } else {
            needToOverload = true;
        }
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
