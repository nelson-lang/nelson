//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <cmath>
#include <limits>
#include <type_traits>
#include "LocalExtrema.hpp"
#include "LocalExtremaCore.hpp"
#include "ErrorID.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
template <class T, class K, class P, class OUT>
static void
localExtremaOfClass(const ArrayOf& A, indexType dim, const LocalExtremaOptions& options,
    NelsonType prominenceClass, ArrayOf& tf, ArrayOf& prominence);
template <class T, class K, class P>
static void
gatherSlice(const T* data, indexType stride, indexType n, const LocalExtremaOptions& options,
    bool needSamplePoints, LocalExtremaWork<K, P>& w);
template <class T, class K>
static K
toKey(T value, bool isMaximum);
template <class P, class OUT>
static P
castMinProminence(double minProminence);
//=============================================================================
void
LocalExtrema(const ArrayOf& A, indexType dim, const LocalExtremaOptions& options, ArrayOf& tf,
    ArrayOf& prominence)
{
    if (A.isComplex()) {
        Error(L"Nelson:isLocalExtrema:ComplexInputArray",
            L"Complex input arguments are not supported.");
    }
    if (A.isSparse()) {
        Error(L"Nelson:isLocalExtrema:SparseInputArray",
            L"Sparse input arguments are not supported.");
    }
    switch (A.dataClass()) {
    case NLS_DOUBLE:
        localExtremaOfClass<double, double, double, double>(
            A, dim, options, NLS_DOUBLE, tf, prominence);
        break;
    case NLS_SINGLE:
        localExtremaOfClass<single, single, single, single>(
            A, dim, options, NLS_SINGLE, tf, prominence);
        break;
    case NLS_LOGICAL:
        localExtremaOfClass<logical, double, double, logical>(
            A, dim, options, NLS_LOGICAL, tf, prominence);
        break;
    case NLS_INT8:
        localExtremaOfClass<int8, int64, uint64_t, uint8>(
            A, dim, options, NLS_UINT8, tf, prominence);
        break;
    case NLS_UINT8:
        localExtremaOfClass<uint8, int64, uint64_t, uint8>(
            A, dim, options, NLS_UINT8, tf, prominence);
        break;
    case NLS_INT16:
        localExtremaOfClass<int16, int64, uint64_t, uint16>(
            A, dim, options, NLS_UINT16, tf, prominence);
        break;
    case NLS_UINT16:
        localExtremaOfClass<uint16, int64, uint64_t, uint16>(
            A, dim, options, NLS_UINT16, tf, prominence);
        break;
    case NLS_INT32:
        localExtremaOfClass<int32, int64, uint64_t, uint32>(
            A, dim, options, NLS_UINT32, tf, prominence);
        break;
    case NLS_UINT32:
        localExtremaOfClass<uint32, int64, uint64_t, uint32>(
            A, dim, options, NLS_UINT32, tf, prominence);
        break;
    case NLS_INT64:
        localExtremaOfClass<int64, int64, uint64_t, uint64>(
            A, dim, options, NLS_UINT64, tf, prominence);
        break;
    case NLS_UINT64:
        localExtremaOfClass<uint64, int64, uint64_t, uint64>(
            A, dim, options, NLS_UINT64, tf, prominence);
        break;
    default:
        Error(L"Nelson:isLocalExtrema:FirstInputInvalid",
            L"First argument must be a numeric or logical array, a table, or a timetable.");
    }
}
//=============================================================================
template <class T, class K, class P, class OUT>
static void
localExtremaOfClass(const ArrayOf& A, indexType dim, const LocalExtremaOptions& options,
    NelsonType prominenceClass, ArrayOf& tf, ArrayOf& prominence)
{
    Dimensions dims = A.dimensions();
    indexType total = dims.elementCount();
    auto* tfData = (logical*)ArrayOf::allocateArrayOf(NLS_LOGICAL, total, stringVector(), true);
    tf = ArrayOf(NLS_LOGICAL, dims, tfData);
    OUT* pData = nullptr;
    if (options.wantProminence) {
        pData = (OUT*)ArrayOf::allocateArrayOf(prominenceClass, total, stringVector(), true);
        prominence = ArrayOf(prominenceClass, dims, pData);
    }
    indexType n = (dim <= dims.length()) ? dims.at(dim - 1) : 1;
    if (total == 0 || n < 3) {
        return;
    }
    indexType stride = 1;
    for (indexType k = 0; k + 1 < dim; ++k) {
        stride *= dims.at(k);
    }
    indexType outer = total / (n * stride);
    const T* data = (const T*)A.dataPointer();
    P minProminence = castMinProminence<P, OUT>(options.minProminence);
    bool needSamplePoints = options.hasWindow || (options.minSeparation > 0);
    LocalExtremaWork<K, P> w;
    for (indexType o = 0; o < outer; ++o) {
        for (indexType i = 0; i < stride; ++i) {
            indexType offset = o * n * stride + i;
            gatherSlice<T, K, P>(data + offset, stride, n, options, needSamplePoints, w);
            localExtremaSlice(w, options, minProminence);
            for (indexType r : w.selected) {
                indexType first = w.runStart[r];
                indexType last = w.runEnd[r];
                if (!w.infinite[first]) {
                    switch (options.flatSelection) {
                    case LocalExtremaFlatSelection::Center:
                        first = (first + last) / 2;
                        last = first;
                        break;
                    case LocalExtremaFlatSelection::First:
                        last = first;
                        break;
                    case LocalExtremaFlatSelection::Last:
                        first = last;
                        break;
                    default:
                        break;
                    }
                }
                for (indexType c = first; c <= last; ++c) {
                    tfData[offset + w.position[c] * stride] = true;
                }
            }
            if (pData == nullptr) {
                continue;
            }
            indexType runs = (indexType)w.runStart.size();
            for (indexType r = 0; r < runs; ++r) {
                if (w.candidate[r]) {
                    for (indexType c = w.runStart[r]; c <= w.runEnd[r]; ++c) {
                        pData[offset + w.position[c] * stride] = (OUT)w.prominence[r];
                    }
                }
            }
        }
    }
}
//=============================================================================
template <class T, class K, class P>
static void
gatherSlice(const T* data, indexType stride, indexType n, const LocalExtremaOptions& options,
    bool needSamplePoints, LocalExtremaWork<K, P>& w)
{
    w.key.resize(n);
    w.position.resize(n);
    w.infinite.resize(n);
    w.x.resize(needSamplePoints ? n : 0);
    const bool isMaximum = options.isMaximum;
    const bool implicitSamplePoints = options.samplePoints.empty();
    indexType m = 0;
    for (indexType k = 0; k < n; ++k) {
        T value = data[k * stride];
        if constexpr (std::is_floating_point<T>::value) {
            if (std::isnan(value)) {
                continue;
            }
        }
        K key = toKey<T, K>(value, isMaximum);
        w.key[m] = key;
        w.position[m] = k;
        if constexpr (std::numeric_limits<K>::has_infinity && !std::is_integral<T>::value) {
            w.infinite[m] = (key == std::numeric_limits<K>::infinity()) ? 1 : 0;
        } else {
            w.infinite[m] = 0;
        }
        if (needSamplePoints) {
            w.x[m] = implicitSamplePoints ? (double)(k + 1) : options.samplePoints[k];
        }
        m++;
    }
    w.key.resize(m);
    w.position.resize(m);
    w.infinite.resize(m);
    w.x.resize(needSamplePoints ? m : 0);
}
//=============================================================================
// Order preserving keys: minima are the maxima of the reversed keys. Integers
// are mapped on signed values (unsigned: sign bit flipped, reversed: bitwise
// not) so that differences of keys are differences of values.
template <class T, class K>
static K
toKey(T value, bool isMaximum)
{
    // logical (an alias of uint8) is the only integer class with a floating key.
    if constexpr (std::is_integral<T>::value && std::is_floating_point<K>::value) {
        return isMaximum ? (K)value : (K)(1 - value);
    } else if constexpr (std::is_floating_point<T>::value) {
        return isMaximum ? (K)value : (K)(-value);
    } else if constexpr (std::is_signed<T>::value) {
        return isMaximum ? (K)value : (K)(T)(~value);
    } else {
        using S = typename std::make_signed<T>::type;
        S shifted = (S)(value ^ (T)((T)1 << (sizeof(T) * 8 - 1)));
        return isMaximum ? (K)shifted : (K)(S)(~shifted);
    }
}
//=============================================================================
// Minimum prominence converted to the class of the prominence output.
template <class P, class OUT>
static P
castMinProminence(double minProminence)
{
    if constexpr (std::is_integral<OUT>::value && std::is_floating_point<P>::value) {
        return (P)(minProminence != 0 ? 1 : 0);
    } else if constexpr (std::is_floating_point<OUT>::value) {
        return (P)(OUT)minProminence;
    } else {
        double rounded = std::round(minProminence);
        double limit = (double)std::numeric_limits<OUT>::max();
        if (rounded >= limit) {
            return (P)std::numeric_limits<OUT>::max();
        }
        return (P)rounded;
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
