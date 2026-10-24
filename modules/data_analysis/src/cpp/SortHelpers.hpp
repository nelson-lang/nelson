//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include <complex>
#include "ParallelSort.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
// The original position makes equivalent values deterministic without changing
// the value-only sort or requiring a stable sorting backend.
template <class Entry, class IsMissing, class Compare>
void
sortIndexedEntries(
    std::vector<Entry>& values, bool missingFirst, IsMissing isMissing, Compare compare)
{
    parallelSort(values.begin(), values.end(), [&](const Entry& a, const Entry& b) {
        const bool aMissing = isMissing(a);
        const bool bMissing = isMissing(b);
        if (aMissing != bMissing) {
            return aMissing == missingFirst;
        }
        if (!aMissing) {
            if (compare(a, b)) {
                return true;
            }
            if (compare(b, a)) {
                return false;
            }
        }
        return a.n < b.n;
    });
}
//=============================================================================
template <class T> class SortRealEntry
{
public:
    double n;
    T x;
};
//=============================================================================
template <class T> class SortComplexEntry
{
public:
    double n;
    std::complex<T> z;
};
//=============================================================================
} // namespace Nelson
//=============================================================================
