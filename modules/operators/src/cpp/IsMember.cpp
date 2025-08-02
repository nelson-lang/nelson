//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#ifdef _MSC_VER
#define _SILENCE_NONFLOATING_COMPLEX_DEPRECATION_WARNING
#endif
//=============================================================================
#include <cstring>
#include "nlsBuildConfig.h"
#include "omp_for_loop.hpp"
#include "lapack_eigen_config.hpp"
#include "IsMember.hpp"
#include "Error.hpp"
//=============================================================================
namespace Nelson {
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
    OMP_PARALLEL_FOR_LOOP(nbElementsA)
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
    return res;
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
    OMP_PARALLEL_FOR_LOOP(stringsA.size())
    for (ompIndexType k = 0; k < (ompIndexType)stringsA.size(); ++k) {
        std::wstring value = stringsA[k];
        for (auto& q : stringsB) {
            if (value == q) {
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
    OMP_PARALLEL_FOR_LOOP(nbElementsA)
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
    OMP_PARALLEL_FOR_LOOP(nbElementsA)
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
    bool isTextA = A.isCharacterArray() || A.isStringArray() || A.isCellArrayOfCharacterVectors();
    bool isTextB = B.isCharacterArray() || B.isStringArray() || B.isCellArrayOfCharacterVectors();
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
