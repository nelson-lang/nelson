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
#include <algorithm>
#include <cmath>
#include <cstring>
#include <utility>
#include <unordered_map>
#include <vector>
#include "nlsBuildConfig.h"
#include "omp_for_loop.hpp"
#include "lapack_eigen_config.hpp"
#include "IsMember.hpp"
#include "Error.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static double*
allocateLocations(const Dimensions& dims, indexType count)
{
    double* locations = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, count);
    memset(locations, 0, sizeof(double) * count);
    return locations;
}
//=============================================================================
static ArrayOf
isMemberStringArray(const ArrayOf& A, const ArrayOf& B, ArrayOf* locationsOut)
{
    logical* elements = (logical*)ArrayOf::allocateArrayOf(NLS_LOGICAL, A.elementCount());
    ArrayOf res = ArrayOf(NLS_LOGICAL, A.dimensions(), elements);
    memset(elements, false, sizeof(logical) * A.elementCount());
    double* locations = nullptr;
    if (locationsOut != nullptr) {
        locations = allocateLocations(A.dimensions(), A.elementCount());
    }
    ArrayOf* dA = (ArrayOf*)A.dataPointer();
    ArrayOf* dB = (ArrayOf*)B.dataPointer();
    ompIndexType nbElementsA = A.elementCount();
    indexType nbElementsB = B.elementCount();
    // First-occurrence index of each distinct B string, so membership is a hash
    // lookup (O(n + m)) instead of a nested scan (O(n * m)). Missing elements
    // (not NLS_CHAR) never match and are excluded from the table.
    std::unordered_map<std::wstring, indexType> firstIndexB;
    firstIndexB.reserve((size_t)nbElementsB * 2 + 1);
    for (indexType q = 0; q < nbElementsB; ++q) {
        if (dB[q].dataClass() == NLS_CHAR) {
            firstIndexB.emplace(dB[q].asWideString(), q + 1);
        }
    }
    OMP_PARALLEL_FOR_LOOP(nbElementsA)
    for (ompIndexType k = 0; k < nbElementsA; ++k) {
        if (dA[k].dataClass() != NLS_CHAR) {
            continue;
        }
        auto it = firstIndexB.find(dA[k].asWideString());
        if (it != firstIndexB.end()) {
            elements[k] = true;
            if (locations != nullptr) {
                locations[k] = (double)it->second;
            }
        }
    }
    if (locationsOut != nullptr) {
        *locationsOut = ArrayOf(NLS_DOUBLE, A.dimensions(), locations);
    }
    return res;
}
//=============================================================================
static ArrayOf
isMemberCharacterArray(const ArrayOf& A, const ArrayOf& B, ArrayOf* locationsOut)
{
    ArrayOf res;
    wstringVector stringsA = A.asWideStringVector(false);
    wstringVector stringsB = B.asWideStringVector(false);
    Dimensions dimsRes(1, stringsA.size());
    if (stringsA.size() == 0) {
        dimsRes.setDimensionLength(0, 0);
    }
    logical* elements = (logical*)ArrayOf::allocateArrayOf(NLS_LOGICAL, stringsA.size());
    res = ArrayOf(NLS_LOGICAL, dimsRes, elements);
    memset(elements, false, sizeof(logical) * stringsA.size());
    double* locations = nullptr;
    if (locationsOut != nullptr) {
        locations = allocateLocations(dimsRes, stringsA.size());
    }
    // First-occurrence index of each distinct B string; membership becomes a
    // hash lookup (O(n + m)) instead of a nested scan (O(n * m)).
    std::unordered_map<std::wstring, indexType> firstIndexB;
    firstIndexB.reserve(stringsB.size() * 2 + 1);
    for (indexType q = 0; q < (indexType)stringsB.size(); ++q) {
        firstIndexB.emplace(stringsB[q], q + 1);
    }
    OMP_PARALLEL_FOR_LOOP(stringsA.size())
    for (ompIndexType k = 0; k < (ompIndexType)stringsA.size(); ++k) {
        auto it = firstIndexB.find(stringsA[k]);
        if (it != firstIndexB.end()) {
            elements[k] = true;
            if (locations != nullptr) {
                locations[k] = (double)it->second;
            }
        }
    }
    if (locationsOut != nullptr) {
        *locationsOut = ArrayOf(NLS_DOUBLE, dimsRes, locations);
    }
    return res;
}
//=============================================================================
template <class T>
ArrayOf
isMemberReal(const ArrayOf& A, const ArrayOf& B, ArrayOf* locationsOut)
{
    logical* elements = (logical*)ArrayOf::allocateArrayOf(NLS_LOGICAL, A.elementCount());
    ArrayOf res = ArrayOf(NLS_LOGICAL, A.dimensions(), elements);
    memset(elements, false, sizeof(logical) * A.elementCount());
    double* locations = nullptr;
    if (locationsOut != nullptr) {
        locations = allocateLocations(A.dimensions(), A.elementCount());
    }
    T* dA = (T*)A.dataPointer();
    T* dB = (T*)B.dataPointer();
    ompIndexType nbElementsA = A.elementCount();
    indexType nbElementsB = B.elementCount();
    // Sort B once (value, then original 1-based index) so each A element is
    // resolved by binary search: O((n+m) log m) instead of O(n*m). NaNs never
    // match, so they are excluded from the sorted table. Equal values keep the
    // lowest original index first, matching the loc contract.
    std::vector<std::pair<T, indexType>> sortedB;
    sortedB.reserve((size_t)nbElementsB);
    for (indexType q = 0; q < nbElementsB; ++q) {
        T bv = dB[q];
        if (bv == bv) {
            sortedB.emplace_back(bv, q + 1);
        }
    }
    std::sort(sortedB.begin(), sortedB.end(),
        [](const std::pair<T, indexType>& a, const std::pair<T, indexType>& b) {
            if (a.first < b.first) {
                return true;
            }
            if (b.first < a.first) {
                return false;
            }
            return a.second < b.second;
        });
    OMP_PARALLEL_FOR_LOOP(nbElementsA)
    for (ompIndexType k = 0; k < nbElementsA; ++k) {
        T value = dA[k];
        if (!(value == value)) {
            continue;
        }
        auto it = std::lower_bound(sortedB.begin(), sortedB.end(), value,
            [](const std::pair<T, indexType>& e, const T& v) { return e.first < v; });
        if (it != sortedB.end() && it->first == value) {
            elements[k] = true;
            if (locations != nullptr) {
                locations[k] = (double)it->second;
            }
        }
    }
    if (locationsOut != nullptr) {
        *locationsOut = ArrayOf(NLS_DOUBLE, A.dimensions(), locations);
    }
    return res;
}
//=============================================================================
template <class T>
ArrayOf
isMemberComplex(const ArrayOf& A, const ArrayOf& B, ArrayOf* locationsOut)
{
    logical* elements = (logical*)ArrayOf::allocateArrayOf(NLS_LOGICAL, A.elementCount());
    ArrayOf res = ArrayOf(NLS_LOGICAL, A.dimensions(), elements);
    memset(elements, false, sizeof(logical) * A.elementCount());
    double* locations = nullptr;
    if (locationsOut != nullptr) {
        locations = allocateLocations(A.dimensions(), A.elementCount());
    }
    T* dA = (T*)A.dataPointer();
    T* dB = (T*)B.dataPointer();
    ompIndexType nbElementsA = A.elementCount();
    OMP_PARALLEL_FOR_LOOP(nbElementsA)
    for (ompIndexType k = 0; k < nbElementsA * 2; k = k + 2) {
        std::complex<T> value(dA[k], dA[k + 1]);
        for (indexType q = 0; q < B.elementCount() * 2; q = q + 2) {
            if (value == std::complex<T>(dB[q], dB[q + 1])) {
                elements[k / 2] = true;
                if (locations != nullptr) {
                    locations[k / 2] = (double)(q / 2 + 1);
                }
                break;
            }
        }
    }
    if (locationsOut != nullptr) {
        *locationsOut = ArrayOf(NLS_DOUBLE, A.dimensions(), locations);
    }
    return res;
}
//=============================================================================
static ArrayOf
IsMemberInternal(const ArrayOf& A, const ArrayOf& B, bool& needToOverload, ArrayOf* locationsOut)
{
    ArrayOf res;
    needToOverload = false;
    bool isTextA = A.isCharacterArray() || A.isStringArray() || A.isCellArrayOfCharacterVectors();
    bool isTextB = B.isCharacterArray() || B.isStringArray() || B.isCellArrayOfCharacterVectors();
    if (A.isCharacterArray() && B.isCharacterArray()) {
        res = isMemberReal<charType>(A, B, locationsOut);
    } else if (isTextA && isTextB) {
        res = isMemberCharacterArray(A, B, locationsOut);
    } else if (A.dataClass() == B.dataClass()) {
        switch (A.dataClass()) {
        case NLS_STRING_ARRAY: {
            res = isMemberStringArray(A, B, locationsOut);
        } break;
        case NLS_LOGICAL: {
            res = isMemberReal<logical>(A, B, locationsOut);
        } break;
        case NLS_UINT8: {
            res = isMemberReal<uint8>(A, B, locationsOut);
        } break;
        case NLS_INT8: {
            res = isMemberReal<int8>(A, B, locationsOut);
        } break;
        case NLS_UINT16: {
            res = isMemberReal<uint16>(A, B, locationsOut);
        } break;
        case NLS_INT16: {
            res = isMemberReal<int16>(A, B, locationsOut);
        } break;
        case NLS_UINT32: {
            res = isMemberReal<uint32>(A, B, locationsOut);
        } break;
        case NLS_INT32: {
            res = isMemberReal<int32>(A, B, locationsOut);
        } break;
        case NLS_UINT64: {
            res = isMemberReal<uint64>(A, B, locationsOut);
        } break;
        case NLS_INT64: {
            res = isMemberReal<int64>(A, B, locationsOut);
        } break;
        case NLS_SINGLE: {
            res = isMemberReal<single>(A, B, locationsOut);
        } break;
        case NLS_DOUBLE: {
            res = isMemberReal<double>(A, B, locationsOut);
        } break;
        case NLS_SCOMPLEX: {
            res = isMemberComplex<single>(A, B, locationsOut);
        } break;
        case NLS_DCOMPLEX: {
            res = isMemberComplex<double>(A, B, locationsOut);
        } break;
        case NLS_CHAR: {
            res = isMemberComplex<charType>(A, B, locationsOut);
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
            res = IsMemberInternal(AA, BB, needToOverload, locationsOut);
        } else {
            needToOverload = true;
        }
    }
    return res;
}
//=============================================================================
ArrayOf
IsMember(const ArrayOf& A, const ArrayOf& B, bool& needToOverload)
{
    return IsMemberInternal(A, B, needToOverload, nullptr);
}
//=============================================================================
ArrayOf
IsMember(const ArrayOf& A, const ArrayOf& B, ArrayOf& locations, bool& needToOverload)
{
    ArrayOf locationsOut;
    ArrayOf res = IsMemberInternal(A, B, needToOverload, &locationsOut);
    if (!needToOverload) {
        locations = locationsOut;
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
