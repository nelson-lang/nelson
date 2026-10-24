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
#include <vector>
#include <algorithm>
#include <limits>
#include "IsMemberTol.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static bool
isSupportedClass(const ArrayOf& V)
{
    switch (V.dataClass()) {
    case NLS_LOGICAL:
    case NLS_INT8:
    case NLS_UINT8:
    case NLS_INT16:
    case NLS_UINT16:
    case NLS_INT32:
    case NLS_UINT32:
    case NLS_INT64:
    case NLS_UINT64:
    case NLS_SINGLE:
    case NLS_DOUBLE:
        return !V.isSparse();
    default:
        return false;
    }
}
//=============================================================================
void
IsMemberTol(const ArrayOf& A, const ArrayOf& B, double tol, bool computeLocb, ArrayOf& lia,
    ArrayOf& locb, bool& needToOverload)
{
    needToOverload = false;
    if (!isSupportedClass(A) || !isSupportedClass(B)) {
        needToOverload = true;
        return;
    }
    bool anySingle = (A.dataClass() == NLS_SINGLE) || (B.dataClass() == NLS_SINGLE);
    if (tol < 0) {
        tol = anySingle ? 1e-6 : 1e-12;
    }
    ArrayOf AA(A);
    ArrayOf BB(B);
    AA.promoteType(NLS_DOUBLE);
    BB.promoteType(NLS_DOUBLE);
    auto* ptrA = (double*)AA.dataPointer();
    auto* ptrB = (double*)BB.dataPointer();
    indexType na = AA.elementCount();
    indexType nb = BB.elementCount();

    // DataScale = max absolute value over finite elements of A and B.
    double ds = 0.0;
    for (indexType i = 0; i < na; ++i) {
        double v = std::fabs(ptrA[i]);
        if (std::isfinite(v) && v > ds) {
            ds = v;
        }
    }
    for (indexType i = 0; i < nb; ++i) {
        double v = std::fabs(ptrB[i]);
        if (std::isfinite(v) && v > ds) {
            ds = v;
        }
    }
    if (!(ds > 0.0) || !std::isfinite(ds)) {
        ds = 1.0;
    }
    double scaledTol = tol * ds;

    // Sorted view of finite B values with their original (lowest) indices.
    std::vector<std::pair<double, indexType>> sortedB;
    sortedB.reserve((size_t)nb);
    for (indexType j = 0; j < nb; ++j) {
        if (std::isfinite(ptrB[j])) {
            sortedB.emplace_back(ptrB[j], j);
        }
    }
    std::sort(sortedB.begin(), sortedB.end(),
        [](const std::pair<double, indexType>& l, const std::pair<double, indexType>& r) {
            return l.first < r.first;
        });
    size_t m = sortedB.size();
    std::vector<double> vals(m);
    for (size_t k = 0; k < m; ++k) {
        vals[k] = sortedB[k].first;
    }

    // Sparse table for range minimum of original indices (only when locb needed).
    std::vector<std::vector<indexType>> rmq;
    std::vector<int> logTable;
    if (computeLocb && m > 0) {
        int maxLog = 1;
        while (((size_t)1 << maxLog) <= m) {
            ++maxLog;
        }
        logTable.assign(m + 1, 0);
        for (size_t i = 2; i <= m; ++i) {
            logTable[i] = logTable[i / 2] + 1;
        }
        rmq.assign(maxLog, std::vector<indexType>(m));
        for (size_t i = 0; i < m; ++i) {
            rmq[0][i] = sortedB[i].second;
        }
        for (int p = 1; ((size_t)1 << p) <= m; ++p) {
            size_t span = ((size_t)1 << p);
            for (size_t i = 0; i + span <= m; ++i) {
                indexType left = rmq[p - 1][i];
                indexType right = rmq[p - 1][i + (span >> 1)];
                rmq[p][i] = (left < right) ? left : right;
            }
        }
    }

    logical* ptrLia = (logical*)ArrayOf::allocateArrayOf(NLS_LOGICAL, na);
    lia = ArrayOf(NLS_LOGICAL, AA.dimensions(), ptrLia);
    double* ptrLocb = nullptr;
    if (computeLocb) {
        ptrLocb = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, na);
        locb = ArrayOf(NLS_DOUBLE, AA.dimensions(), ptrLocb);
    }

    for (indexType i = 0; i < na; ++i) {
        ptrLia[i] = (logical)0;
        if (computeLocb) {
            ptrLocb[i] = 0.0;
        }
        double a = ptrA[i];
        if (m == 0 || !std::isfinite(a)) {
            continue;
        }
        double lowVal = a - scaledTol;
        double highVal = a + scaledTol;
        size_t lo = (size_t)(std::lower_bound(vals.begin(), vals.end(), lowVal) - vals.begin());
        if (lo >= m || vals[lo] > highVal) {
            continue;
        }
        ptrLia[i] = (logical)1;
        if (computeLocb) {
            size_t hi
                = (size_t)(std::upper_bound(vals.begin(), vals.end(), highVal) - vals.begin());
            // range minimum of original indices over [lo, hi)
            size_t len = hi - lo;
            int p = logTable[len];
            indexType left = rmq[p][lo];
            indexType right = rmq[p][hi - ((size_t)1 << p)];
            indexType best = (left < right) ? left : right;
            ptrLocb[i] = (double)(best + 1);
        }
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
