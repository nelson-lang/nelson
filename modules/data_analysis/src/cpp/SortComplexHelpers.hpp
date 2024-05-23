//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include <complex>
#include <algorithm>
#include <vector>
#include "Types.hpp"
#include "SortHelpers.hpp"
#include "Sort.hpp"
#include "nlsBuildConfig.h"
#include "ParallelSort.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
struct
{
    template <class T>
    bool
    operator()(const std::complex<T>& a, const std::complex<T>& b)
    {

        if (std::isnan(a.real()) || std::isnan(a.imag())) {
            return false;
        }

        if (std::isnan(b.real()) || std::isnan(b.imag())) {
            return true;
        }
        if (a.real() == b.real()) {
            return a.imag() < b.imag();
        }
        return a.real() < b.real();
    }
} comparisonComplexLess;
//=============================================================================
struct
{
    template <class T>
    bool
    operator()(const std::complex<T>& a, const std::complex<T>& b)
    {
        T a_abs = std::abs(a);
        T b_abs = std::abs(b);

        if (std::isnan(b_abs) && !std::isnan(a_abs)) {
            return true;
        }
        if (a_abs < b_abs) {
            return true;
        }
        if (a_abs == b_abs) {
            return atan2(a.imag(), a.real()) < atan2(b.imag(), b.real());
        }
        return false;
    }
} comparisonComplexAbsoluteValueLess;
//=============================================================================
struct
{
    template <class T>
    bool
    operator()(const std::complex<T>& a, const std::complex<T>& b)
    {
        if (std::isnan(a.real()) && std::isnan(b.real()) || (a.real() == b.real())) {
            if (std::isnan(a.imag()) && !std::isnan(b.imag())) {
                return true;
            }
            return b.imag() < a.imag();
        }
        if (std::isnan(a.real()) && !std::isnan(b.real())) {
            return true;
        }
        return b.real() < a.real();
    }
} comparisonComplexGreater;
//=============================================================================
struct
{
    template <class T>
    bool
    operator()(const std::complex<T>& a, const std::complex<T>& b)
    {
        T a_abs = std::abs(a);
        T b_abs = std::abs(b);

        if (std::isnan(a_abs) && !std::isnan(b_abs)) {
            return true;
        }
        if (b_abs < a_abs) {
            return true;
        }
        if (a_abs == b_abs) {
            return atan2(b.imag(), b.real()) < atan2(a.imag(), a.real());
        }
        return false;
    }
} comparisonComplexAbsoluteValueGreater;
//=============================================================================
struct
{
    template <class T>
    bool
    operator()(const SortComplexEntry<T>& a, const SortComplexEntry<T>& b)
    {
        return comparisonComplexLess(a.z, b.z);
    }
} comparisonComplexEntryLess;
//=============================================================================
struct
{
    template <class T>
    bool
    operator()(const SortComplexEntry<T>& a, const SortComplexEntry<T>& b)
    {
        return comparisonComplexAbsoluteValueLess(a.z, b.z);
    }
} comparisonComplexEntryAbsoluteValueLess;
//=============================================================================
struct
{
    template <class T>
    bool
    operator()(const SortComplexEntry<T>& a, const SortComplexEntry<T>& b)
    {
        return comparisonComplexGreater(a.z, b.z);
    }
} comparisonComplexEntryGreater;
//=============================================================================
struct
{
    template <class T>
    bool
    operator()(const SortComplexEntry<T>& a, const SortComplexEntry<T>& b)
    {
        return comparisonComplexAbsoluteValueGreater(a.z, b.z);
    }
} comparisonComplexEntryAbsoluteValueGreater;
//=============================================================================
template <class T>
bool
isPureZero(std::complex<T> n)
{
    return n.real() == 0. && n.imag() == 0.;
}
//=============================================================================
template <class T>
void
VectorComplexSortWithoutIndexMissingAuto(const T* sp, T* dp, indexType planes, indexType planesize,
    indexType linesize, bool ascend, COMPARISON_METHOD comparisonMethod)
{
    std::complex<T>* z = reinterpret_cast<std::complex<T>*>(dp);
    if (ascend) {
        switch (comparisonMethod) {
        case COMPARISON_METHOD::AUTO_METHOD: {
            parallelSort(z, z + linesize, comparisonComplexAbsoluteValueLess);
        } break;
        case COMPARISON_METHOD::REAL_METHOD: {
            std::complex<T>* pt = std::stable_partition(z, z + linesize,
                [](const std::complex<T>& i) { return (!isPureZero(i) || i.imag() == 0.); });
            if (pt == z) {
                parallelSort(z, z + linesize, comparisonComplexLess);
            } else {
                parallelSort(z, pt, comparisonComplexLess);
            }
        } break;
        case COMPARISON_METHOD::ABS_METHOD: {
            parallelSort(z, z + linesize, comparisonComplexAbsoluteValueLess);
        } break;
        }
    } else {
        std::complex<T>* pt = std::partition(z, z + linesize,
            [](const std::complex<T>& i) { return std::isnan(i.real()) || std::isnan(i.imag()); });
        switch (comparisonMethod) {
        case COMPARISON_METHOD::AUTO_METHOD: {
            parallelSort(pt, z + linesize, comparisonComplexAbsoluteValueGreater);
        } break;
        case COMPARISON_METHOD::REAL_METHOD: {
            parallelSort(pt, z + linesize, comparisonComplexGreater);
        } break;
        case COMPARISON_METHOD::ABS_METHOD: {
            parallelSort(pt, z + linesize, comparisonComplexAbsoluteValueGreater);
        } break;
        }
    }
}
//=============================================================================
template <class T>
void
VectorComplexSortWithoutIndexMissingFirst(const T* sp, T* dp, indexType planes, indexType planesize,
    indexType linesize, bool ascend, COMPARISON_METHOD comparisonMethod)
{
    std::complex<T>* z = reinterpret_cast<std::complex<T>*>(dp);
    std::complex<T>* pt = std::partition(z, z + linesize,
        [](const std::complex<T>& i) { return std::isnan(i.real()) || std::isnan(i.imag()); });
    if (ascend) {
        switch (comparisonMethod) {
        case COMPARISON_METHOD::AUTO_METHOD: {
            parallelSort(pt, z + linesize, comparisonComplexAbsoluteValueLess);
        } break;
        case COMPARISON_METHOD::REAL_METHOD: {
            parallelSort(pt, z + linesize, comparisonComplexLess);
        } break;
        case COMPARISON_METHOD::ABS_METHOD: {
            parallelSort(pt, z + linesize, comparisonComplexAbsoluteValueLess);
        } break;
        }
    } else {
        switch (comparisonMethod) {
        case COMPARISON_METHOD::AUTO_METHOD: {
            parallelSort(pt, z + linesize, comparisonComplexAbsoluteValueGreater);
        } break;
        case COMPARISON_METHOD::REAL_METHOD: {
            parallelSort(pt, z + linesize, comparisonComplexGreater);
        } break;
        case COMPARISON_METHOD::ABS_METHOD: {
            parallelSort(pt, z + linesize, comparisonComplexAbsoluteValueGreater);
        } break;
        }
    }
}
//=============================================================================
template <class T>
void
VectorComplexSortWithoutIndexMissingLast(const T* sp, T* dp, indexType planes, indexType planesize,
    indexType linesize, bool ascend, COMPARISON_METHOD comparisonMethod)
{
    std::complex<T>* z = reinterpret_cast<std::complex<T>*>(dp);
    std::complex<T>* pt = std::partition(z, z + linesize,
        [](const std::complex<T>& i) { return !std::isnan(i.real()) && !std::isnan(i.imag()); });
    if (ascend) {
        switch (comparisonMethod) {
        case COMPARISON_METHOD::AUTO_METHOD: {
            parallelSort(pt, z + linesize, comparisonComplexAbsoluteValueLess);
        } break;
        case COMPARISON_METHOD::REAL_METHOD: {
            parallelSort(pt, z + linesize, comparisonComplexLess);
        } break;
        case COMPARISON_METHOD::ABS_METHOD: {
            parallelSort(pt, z + linesize, comparisonComplexAbsoluteValueLess);
        } break;
        }
    } else {
        switch (comparisonMethod) {
        case COMPARISON_METHOD::AUTO_METHOD: {
            parallelSort(pt, z + linesize, comparisonComplexAbsoluteValueGreater);
        } break;
        case COMPARISON_METHOD::REAL_METHOD: {
            parallelSort(pt, z + linesize, comparisonComplexGreater);
        } break;
        case COMPARISON_METHOD::ABS_METHOD: {
            parallelSort(pt, z + linesize, comparisonComplexAbsoluteValueGreater);
        } break;
        }
    }
}
//=============================================================================
template <class T>
void
MatrixComplexSortWithoutIndexMissingAuto(
    std::vector<std::complex<T>>& buf, bool ascend, COMPARISON_METHOD comparisonMethod)
{
    if (ascend) {
        switch (comparisonMethod) {
        case COMPARISON_METHOD::AUTO_METHOD: {
            parallelSort(buf.begin(), buf.end(), comparisonComplexAbsoluteValueLess);
        } break;
        case COMPARISON_METHOD::REAL_METHOD: {
            parallelSort(buf.begin(), buf.end(), comparisonComplexLess);
        } break;
        case COMPARISON_METHOD::ABS_METHOD: {
            parallelSort(buf.begin(), buf.end(), comparisonComplexAbsoluteValueLess);
        } break;
        }
    } else {
        auto pt = std::partition(buf.begin(), buf.end(),
            [](const std::complex<T>& i) { return std::isnan(i.real()) || std::isnan(i.imag()); });
        switch (comparisonMethod) {
        case COMPARISON_METHOD::AUTO_METHOD: {
            parallelSort(pt, buf.end(), comparisonComplexAbsoluteValueGreater);
        } break;
        case COMPARISON_METHOD::REAL_METHOD: {
            parallelSort(pt, buf.end(), comparisonComplexGreater);
        } break;
        case COMPARISON_METHOD::ABS_METHOD: {
            parallelSort(pt, buf.end(), comparisonComplexAbsoluteValueGreater);
        } break;
        }
    }
}
//=============================================================================
template <class T>
void
MatrixComplexSortWithoutIndexMissingFirst(
    std::vector<std::complex<T>>& buf, bool ascend, COMPARISON_METHOD comparisonMethod)
{
    auto pt = std::partition(buf.begin(), buf.end(),
        [](const std::complex<T>& i) { return std::isnan(i.real()) || std::isnan(i.imag()); });
    if (ascend) {
        switch (comparisonMethod) {
        case COMPARISON_METHOD::AUTO_METHOD: {
            parallelSort(pt, buf.end(), comparisonComplexAbsoluteValueLess);
        } break;
        case COMPARISON_METHOD::REAL_METHOD: {
            parallelSort(pt, buf.end(), comparisonComplexLess);
        } break;
        case COMPARISON_METHOD::ABS_METHOD: {
            parallelSort(pt, buf.end(), comparisonComplexAbsoluteValueLess);
        } break;
        }
    } else {
        switch (comparisonMethod) {
        case COMPARISON_METHOD::AUTO_METHOD: {
            parallelSort(pt, buf.end(), comparisonComplexAbsoluteValueGreater);
        } break;
        case COMPARISON_METHOD::REAL_METHOD: {
            parallelSort(pt, buf.end(), comparisonComplexGreater);
        } break;
        case COMPARISON_METHOD::ABS_METHOD: {
            parallelSort(pt, buf.end(), comparisonComplexAbsoluteValueGreater);
        } break;
        }
    }
}
//=============================================================================
template <class T>
void
MatrixComplexSortWithoutIndexMissingLast(
    std::vector<std::complex<T>>& buf, bool ascend, COMPARISON_METHOD comparisonMethod)
{

    auto pt = std::partition(buf.begin(), buf.end(),
        [](const std::complex<T>& i) { return !std::isnan(i.real()) && !std::isnan(i.imag()); });
    if (ascend) {
        switch (comparisonMethod) {
        case COMPARISON_METHOD::AUTO_METHOD: {
            parallelSort(pt, buf.end(), comparisonComplexAbsoluteValueLess);
        } break;
        case COMPARISON_METHOD::REAL_METHOD: {
            parallelSort(pt, buf.end(), comparisonComplexLess);
        } break;
        case COMPARISON_METHOD::ABS_METHOD: {
            parallelSort(pt, buf.end(), comparisonComplexAbsoluteValueLess);
        } break;
        }
    } else {
        switch (comparisonMethod) {
        case COMPARISON_METHOD::AUTO_METHOD: {
            parallelSort(pt, buf.end(), comparisonComplexAbsoluteValueGreater);
        } break;
        case COMPARISON_METHOD::REAL_METHOD: {
            parallelSort(pt, buf.end(), comparisonComplexGreater);
        } break;
        case COMPARISON_METHOD::ABS_METHOD: {
            parallelSort(pt, buf.end(), comparisonComplexAbsoluteValueGreater);
        } break;
        }
    }
}
//=============================================================================
template <class T>
void
ComplexEntrySortByPlacementAuto(
    std::vector<SortComplexEntry<T>>& buf, bool ascend, COMPARISON_METHOD comparisonMethod)
{
    if (ascend) {
        switch (comparisonMethod) {
        case COMPARISON_METHOD::AUTO_METHOD: {
            parallelSort(buf.begin(), buf.end(), comparisonComplexEntryAbsoluteValueLess);
        } break;
        case COMPARISON_METHOD::REAL_METHOD: {
            auto pt
                = std::stable_partition(buf.begin(), buf.end(), [](const SortComplexEntry<T>& i) {
                      return (!(i.z.real() == 0. && i.z.imag() == 0.) || (i.z.imag() == 0.));
                  });
            if (pt == buf.begin()) {
                parallelSort(buf.begin(), buf.end(), comparisonComplexEntryLess);
            } else {
                parallelSort(buf.begin(), pt, comparisonComplexEntryLess);
            }
        } break;
        case COMPARISON_METHOD::ABS_METHOD: {
            parallelSort(buf.begin(), buf.end(), comparisonComplexEntryAbsoluteValueLess);
        } break;
        }
    } else {
        auto it = std::partition(buf.begin(), buf.end(), [](const SortComplexEntry<T>& i) {
            return std::isnan(i.z.real()) || std::isnan(i.z.imag());
        });

        parallelSort(
            buf.begin(), buf.end(), [](const SortComplexEntry<T>& a, const SortComplexEntry<T>& b) {
                if ((std::isnan(a.z.real()) || std::isnan(a.z.imag()))
                    && (std::isnan(b.z.real()) || std::isnan(b.z.imag()))) {
                    return a.n < b.n;
                }
                return a.n != a.n;
            });
        switch (comparisonMethod) {
        case COMPARISON_METHOD::AUTO_METHOD: {
            parallelSort(it, buf.end(), comparisonComplexEntryAbsoluteValueGreater);
        } break;
        case COMPARISON_METHOD::REAL_METHOD: {
            parallelSort(it, buf.end(), comparisonComplexEntryGreater);
        } break;
        case COMPARISON_METHOD::ABS_METHOD: {
            parallelSort(it, buf.end(), comparisonComplexEntryAbsoluteValueGreater);
        } break;
        }
    }
}
//=============================================================================
template <class T>
void
ComplexEntrySortByPlacementFirst(
    std::vector<SortComplexEntry<T>>& buf, bool ascend, COMPARISON_METHOD comparisonMethod)
{
    auto it = std::partition(buf.begin(), buf.end(), [](const SortComplexEntry<T>& i) {
        return std::isnan(i.z.real()) || std::isnan(i.z.imag());
    });
    parallelSort(
        buf.begin(), buf.end(), [](const SortComplexEntry<T>& a, const SortComplexEntry<T>& b) {
            if ((std::isnan(a.z.real()) || std::isnan(a.z.imag()))
                && (std::isnan(b.z.real()) || std::isnan(b.z.imag()))) {
                return a.n < b.n;
            }
            return a.n != a.n;
        });

    if (ascend) {
        switch (comparisonMethod) {
        case COMPARISON_METHOD::AUTO_METHOD: {
            parallelSort(it, buf.end(), comparisonComplexEntryAbsoluteValueLess);
        } break;
        case COMPARISON_METHOD::REAL_METHOD: {
            parallelSort(it, buf.end(), comparisonComplexEntryLess);
        } break;
        case COMPARISON_METHOD::ABS_METHOD: {
            parallelSort(it, buf.end(), comparisonComplexEntryAbsoluteValueLess);
        } break;
        }
    } else {
        switch (comparisonMethod) {
        case COMPARISON_METHOD::AUTO_METHOD: {
            parallelSort(it, buf.end(), comparisonComplexEntryAbsoluteValueGreater);
        } break;
        case COMPARISON_METHOD::REAL_METHOD: {
            parallelSort(it, buf.end(), comparisonComplexEntryGreater);
        } break;
        case COMPARISON_METHOD::ABS_METHOD: {
            parallelSort(it, buf.end(), comparisonComplexEntryAbsoluteValueGreater);
        } break;
        }
    }
}
//=============================================================================
template <class T>
void
ComplexEntrySortByPlacementLast(
    std::vector<SortComplexEntry<T>>& buf, bool ascend, COMPARISON_METHOD comparisonMethod)
{
    auto it = std::partition(buf.begin(), buf.end(), [](const SortComplexEntry<T>& i) {
        return !std::isnan(i.z.real()) && !std::isnan(i.z.imag());
    });
    parallelSort(
        buf.begin(), buf.end(), [](const SortComplexEntry<T>& a, const SortComplexEntry<T>& b) {
            if ((std::isnan(a.z.real()) || std::isnan(a.z.imag()))
                && (std::isnan(b.z.real()) || std::isnan(b.z.imag()))) {
                return a.n < b.n;
            }
            return a.n != a.n;
        });

    if (ascend) {
        switch (comparisonMethod) {
        case COMPARISON_METHOD::AUTO_METHOD: {
            parallelSort(it, buf.end(), comparisonComplexEntryAbsoluteValueLess);
        } break;
        case COMPARISON_METHOD::REAL_METHOD: {
            parallelSort(it, buf.end(), comparisonComplexEntryLess);
        } break;
        case COMPARISON_METHOD::ABS_METHOD: {
            parallelSort(it, buf.end(), comparisonComplexEntryAbsoluteValueLess);
        } break;
        }
    } else {
        switch (comparisonMethod) {
        case COMPARISON_METHOD::AUTO_METHOD: {
            parallelSort(it, buf.end(), comparisonComplexEntryAbsoluteValueGreater);
        } break;
        case COMPARISON_METHOD::REAL_METHOD: {
            parallelSort(it, buf.end(), comparisonComplexEntryGreater);
        } break;
        case COMPARISON_METHOD::ABS_METHOD: {
            parallelSort(it, buf.end(), comparisonComplexEntryAbsoluteValueGreater);
        } break;
        }
    }
}
//=============================================================================
template <class T>
void
ComplexEntrySortByPlacement(std::vector<SortComplexEntry<T>>& buf, bool ascend,
    MISSING_PLACEMENT placement, COMPARISON_METHOD comparisonMethod)
{
    switch (placement) {
    case MISSING_PLACEMENT::AUTO_PLACEMENT: {
        ComplexEntrySortByPlacementAuto(buf, ascend, comparisonMethod);
    } break;
    case MISSING_PLACEMENT::FIRST_PLACEMENT: {
        ComplexEntrySortByPlacementFirst(buf, ascend, comparisonMethod);
    } break;
    case MISSING_PLACEMENT::LAST_PLACEMENT: {
        ComplexEntrySortByPlacementLast(buf, ascend, comparisonMethod);
    } break;
    }
}
//=============================================================================
template <class T>
void
ComplexSortWithIndex(const T* sp, T* dp, double* ip, indexType planes, indexType planesize,
    indexType linesize, bool isVector, bool ascend, MISSING_PLACEMENT placement,
    COMPARISON_METHOD comparisonMethod)
{
    std::vector<SortComplexEntry<T>> buf;
    buf.reserve(linesize);
    if (isVector) {
        for (indexType i = 0; i < linesize; i++) {
            SortComplexEntry<T> entry;
            std::complex<T> val(sp[2 * i], sp[(2 * i) + 1]);
            entry.z = val;
            entry.n = (double)i + 1;
            buf.push_back(entry);
        }
        ComplexEntrySortByPlacement(buf, ascend, placement, comparisonMethod);
        for (indexType i = 0; i < linesize; i++) {
            dp[2 * i] = buf[i].z.real();
            dp[(2 * i) + 1] = buf[i].z.imag();
            ip[i] = buf[i].n;
        }
    } else {
        for (indexType i = 0; i < planes; i++) {
            for (indexType j = 0; j < planesize; j++) {
                buf.clear();
                for (indexType k = 0; k < linesize; k++) {
                    SortComplexEntry<T> entry;
                    T rp = sp[2 * (i * planesize * linesize + j + k * planesize)];
                    T ipart = sp[2 * (i * planesize * linesize + j + k * planesize) + 1];
                    std::complex<T> val(rp, ipart);
                    entry.z = val;
                    entry.n = (double)k + 1;
                    buf.push_back(entry);
                }
                ComplexEntrySortByPlacement(buf, ascend, placement, comparisonMethod);
                for (indexType k = 0; k < linesize; k++) {

                    dp[2 * (i * planesize * linesize + j + k * planesize)] = buf[k].z.real();
                    dp[2 * (i * planesize * linesize + j + k * planesize) + 1] = buf[k].z.imag();
                    ip[i * planesize * linesize + j + k * planesize] = buf[k].n;
                }
            }
        }
    }
}
//=============================================================================
template <class T>
void
VectorComplexSortWithoutIndex(const T* sp, T* dp, indexType planes, indexType planesize,
    indexType linesize, bool ascend, MISSING_PLACEMENT placement,
    COMPARISON_METHOD comparisonMethod)
{
    switch (placement) {
    case MISSING_PLACEMENT::AUTO_PLACEMENT: {
        VectorComplexSortWithoutIndexMissingAuto(
            sp, dp, planes, planesize, linesize, ascend, comparisonMethod);
    } break;
    case MISSING_PLACEMENT::FIRST_PLACEMENT: {
        VectorComplexSortWithoutIndexMissingFirst(
            sp, dp, planes, planesize, linesize, ascend, comparisonMethod);
    } break;
    case MISSING_PLACEMENT::LAST_PLACEMENT: {
        VectorComplexSortWithoutIndexMissingLast(
            sp, dp, planes, planesize, linesize, ascend, comparisonMethod);
    } break;
    }
}
//=============================================================================
template <class T>
void
MatrixComplexSortWithoutIndex(std::vector<std::complex<T>>& buf, bool ascend,
    MISSING_PLACEMENT placement, COMPARISON_METHOD comparisonMethod)
{
    switch (placement) {
    case MISSING_PLACEMENT::AUTO_PLACEMENT: {
        MatrixComplexSortWithoutIndexMissingAuto<T>(buf, ascend, comparisonMethod);
    } break;
    case MISSING_PLACEMENT::FIRST_PLACEMENT: {
        MatrixComplexSortWithoutIndexMissingFirst<T>(buf, ascend, comparisonMethod);
    } break;
    case MISSING_PLACEMENT::LAST_PLACEMENT: {
        MatrixComplexSortWithoutIndexMissingLast<T>(buf, ascend, comparisonMethod);
    } break;
    }
}
//=============================================================================
template <class T>
void
ComplexSortWithoutIndex(const T* sp, T* dp, indexType planes, indexType planesize,
    indexType linesize, bool isVector, bool ascend, MISSING_PLACEMENT placement,
    COMPARISON_METHOD comparisonMethod)
{
    if (isVector) {
        VectorComplexSortWithoutIndex(
            sp, dp, planes, planesize, linesize, ascend, placement, comparisonMethod);
    } else {
        std::vector<std::complex<T>> buf;
        buf.reserve(linesize);
        for (indexType i = 0; i < planes; i++) {
            for (indexType j = 0; j < planesize; j++) {
                buf.clear();
                for (indexType k = 0; k < linesize; k++) {
                    T rp = sp[2 * (i * planesize * linesize + j + k * planesize)];
                    T ip = sp[2 * (i * planesize * linesize + j + k * planesize) + 1];
                    std::complex<T> value(rp, ip);
                    buf.push_back(value);
                }

                MatrixComplexSortWithoutIndex(buf, ascend, placement, comparisonMethod);

                for (indexType k = 0; k < linesize; k++) {
                    dp[2 * (i * planesize * linesize + j + k * planesize)] = buf[k].real();
                    dp[2 * (i * planesize * linesize + j + k * planesize) + 1] = buf[k].imag();
                }
            }
        }
    }
}
//=============================================================================
template <class T>
ArrayOfVector
sortComplex(const ArrayOf& arrayIn, NelsonType dataClass, bool withIndex, indexType linesize,
    indexType planecount, indexType planesize, Dimensions& outDim, indexType dim, bool ascend,
    MISSING_PLACEMENT placement, COMPARISON_METHOD comparisonMethod)
{
    ArrayOfVector res;
    ArrayOf sortedValues, indexValues;
    bool isVector = arrayIn.isVector();
    if (withIndex) {
        T* ptrValue = nullptr;
        if (isVector) {
            sortedValues = ArrayOf(arrayIn);
            sortedValues.ensureSingleOwner();
            ptrValue = (T*)sortedValues.getDataPointer();
        } else {
            ptrValue = (T*)ArrayOf::allocateArrayOf(
                dataClass, outDim.getElementCount(), stringVector(), false);
            sortedValues = ArrayOf(dataClass, outDim, ptrValue);
        }
        double* ptrIndex = (double*)ArrayOf::allocateArrayOf(
            NLS_DOUBLE, outDim.getElementCount(), stringVector(), false);
        if (isVector) {
            ompIndexType elementCount = (ompIndexType)outDim.getElementCount();
#if WITH_OPENMP
#pragma omp parallel for
#endif
            for (ompIndexType k = 0; k < elementCount; ++k) {
                ptrIndex[k] = (double)1;
            }
        }
        indexValues = ArrayOf(NLS_DOUBLE, outDim, ptrIndex);
        ComplexSortWithIndex<T>((const T*)arrayIn.getDataPointer(), (T*)ptrValue, ptrIndex,
            planecount, planesize, linesize, isVector, ascend, placement, comparisonMethod);

        res.push_back(sortedValues);
        res.push_back(indexValues);
    } else {
        T* ptrValue = nullptr;
        if (isVector) {
            sortedValues = ArrayOf(arrayIn);
            sortedValues.ensureSingleOwner();
            ptrValue = (T*)sortedValues.getDataPointer();
        } else {
            ptrValue = (T*)ArrayOf::allocateArrayOf(
                dataClass, outDim.getElementCount(), stringVector(), false);
            sortedValues = ArrayOf(dataClass, outDim, ptrValue);
        }
        ComplexSortWithoutIndex((const T*)arrayIn.getDataPointer(), (T*)ptrValue, planecount,
            planesize, linesize, isVector, ascend, placement, comparisonMethod);
        res.push_back(sortedValues);
    }
    return res;
}
//=============================================================================
}
//=============================================================================
