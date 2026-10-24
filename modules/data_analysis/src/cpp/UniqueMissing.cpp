//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <algorithm>
#include <cmath>
#include <numeric>
#include <vector>
#include "UniqueMissing.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static bool
hasMissingValues(NelsonType cls);
static bool
isMissingAt(const ArrayOf& C, indexType k);
static int
compareAt(const ArrayOf& C, indexType i, indexType j);
static int
compareRows(const ArrayOf& C, indexType r1, indexType r2, indexType nRows, indexType nCols);
static ArrayOf
selectUnique(const ArrayOf& C, const std::vector<indexType>& keep, bool asRows);
//=============================================================================
ArrayOfVector
UniqueMergeMissing(const ArrayOfVector& base, bool asRows)
{
    if (base.size() < 3 || !hasMissingValues(base[0].dataClass())) {
        return base;
    }
    const ArrayOf& C = base[0];
    indexType G = base[1].elementCount();
    indexType nRows = asRows ? C.rows() : G;
    indexType nCols = asRows ? C.columns() : 1;
    std::vector<indexType> candidates;
    for (indexType g = 0; g < G; ++g) {
        for (indexType c = 0; c < nCols; ++c) {
            if (isMissingAt(C, g + c * nRows)) {
                candidates.push_back(g);
                break;
            }
        }
    }
    if (candidates.size() < 2) {
        return base;
    }
    // equal keys become adjacent, in increasing position order (stable sort):
    // the representative of a group is its first position in C.
    std::stable_sort(candidates.begin(), candidates.end(),
        [&](indexType a, indexType b) { return compareRows(C, a, b, nRows, nCols) < 0; });
    std::vector<indexType> representative(G);
    std::iota(representative.begin(), representative.end(), (indexType)0);
    for (size_t k = 1; k < candidates.size(); ++k) {
        if (compareRows(C, candidates[k - 1], candidates[k], nRows, nCols) == 0) {
            representative[candidates[k]] = representative[candidates[k - 1]];
        }
    }
    const double* ia = (const double*)base[1].dataPointer();
    const double* ic = (const double*)base[2].dataPointer();
    std::vector<indexType> newPosition(G, 0);
    std::vector<indexType> keep;
    for (indexType g = 0; g < G; ++g) {
        if (representative[g] == g) {
            newPosition[g] = keep.size();
            keep.push_back(g);
        }
    }
    indexType K = keep.size();
    double* newIa = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, K);
    for (indexType k = 0; k < K; ++k) {
        newIa[k] = ia[keep[k]];
    }
    for (indexType g = 0; g < G; ++g) {
        indexType r = newPosition[representative[g]];
        newIa[r] = std::min(newIa[r], ia[g]);
    }
    indexType N = base[2].elementCount();
    double* newIc = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, N);
    for (indexType k = 0; k < N; ++k) {
        indexType g = (indexType)ic[k] - 1;
        newIc[k] = (double)(newPosition[representative[g]] + 1);
    }
    ArrayOfVector retval;
    retval << selectUnique(C, keep, asRows);
    retval << ArrayOf(NLS_DOUBLE, Dimensions(K, 1), newIa);
    retval << ArrayOf(NLS_DOUBLE, base[2].dimensions(), newIc);
    return retval;
}
//=============================================================================
template <class T>
static int
compareRealAt(const T* p, indexType i, indexType j)
{
    bool mi = std::isnan(p[i]);
    bool mj = std::isnan(p[j]);
    if (mi || mj) {
        return (mi == mj) ? 0 : (mi ? 1 : -1);
    }
    return (p[i] < p[j]) ? -1 : ((p[j] < p[i]) ? 1 : 0);
}
//=============================================================================
template <class T>
static int
compareComplexAt(const T* p, indexType i, indexType j)
{
    bool mi = std::isnan(p[2 * i]) || std::isnan(p[2 * i + 1]);
    bool mj = std::isnan(p[2 * j]) || std::isnan(p[2 * j + 1]);
    if (mi || mj) {
        return (mi == mj) ? 0 : (mi ? 1 : -1);
    }
    int r = compareRealAt<T>(p, 2 * i, 2 * j);
    return (r != 0) ? r : compareRealAt<T>(p, 2 * i + 1, 2 * j + 1);
}
//=============================================================================
static int
compareStringAt(const ArrayOf* p, indexType i, indexType j)
{
    bool mi = !p[i].isCharacterArray();
    bool mj = !p[j].isCharacterArray();
    if (mi || mj) {
        return (mi == mj) ? 0 : (mi ? 1 : -1);
    }
    return p[i].asWideString().compare(p[j].asWideString());
}
//=============================================================================
static bool
hasMissingValues(NelsonType cls)
{
    return cls == NLS_DOUBLE || cls == NLS_SINGLE || cls == NLS_DCOMPLEX || cls == NLS_SCOMPLEX
        || cls == NLS_STRING_ARRAY;
}
//=============================================================================
static bool
isMissingAt(const ArrayOf& C, indexType k)
{
    const void* p = C.dataPointer();
    switch (C.dataClass()) {
    case NLS_DOUBLE:
        return std::isnan(((const double*)p)[k]);
    case NLS_SINGLE:
        return std::isnan(((const single*)p)[k]);
    case NLS_DCOMPLEX:
        return std::isnan(((const double*)p)[2 * k]) || std::isnan(((const double*)p)[2 * k + 1]);
    case NLS_SCOMPLEX:
        return std::isnan(((const single*)p)[2 * k]) || std::isnan(((const single*)p)[2 * k + 1]);
    case NLS_STRING_ARRAY:
        return !((const ArrayOf*)p)[k].isCharacterArray();
    default:
        return false;
    }
}
//=============================================================================
static int
compareAt(const ArrayOf& C, indexType i, indexType j)
{
    const void* p = C.dataPointer();
    switch (C.dataClass()) {
    case NLS_DOUBLE:
        return compareRealAt<double>((const double*)p, i, j);
    case NLS_SINGLE:
        return compareRealAt<single>((const single*)p, i, j);
    case NLS_DCOMPLEX:
        return compareComplexAt<double>((const double*)p, i, j);
    case NLS_SCOMPLEX:
        return compareComplexAt<single>((const single*)p, i, j);
    case NLS_STRING_ARRAY:
        return compareStringAt((const ArrayOf*)p, i, j);
    default:
        return 0;
    }
}
//=============================================================================
static int
compareRows(const ArrayOf& C, indexType r1, indexType r2, indexType nRows, indexType nCols)
{
    for (indexType c = 0; c < nCols; ++c) {
        int r = compareAt(C, r1 + c * nRows, r2 + c * nRows);
        if (r != 0) {
            return r;
        }
    }
    return 0;
}
//=============================================================================
static ArrayOf
selectUnique(const ArrayOf& C, const std::vector<indexType>& keep, bool asRows)
{
    indexType K = keep.size();
    double* idx = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, K);
    for (indexType k = 0; k < K; ++k) {
        idx[k] = (double)(keep[k] + 1);
    }
    ArrayOf index(NLS_DOUBLE, Dimensions(K, 1), idx);
    ArrayOf source(C);
    if (!asRows) {
        return source.vectorSubset(index);
    }
    indexType cols = C.columns();
    double* allCols = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, cols);
    for (indexType c = 0; c < cols; ++c) {
        allCols[c] = (double)(c + 1);
    }
    ArrayOfVector subscripts;
    subscripts << index;
    subscripts << ArrayOf(NLS_DOUBLE, Dimensions(1, cols), allCols);
    return source.nDimSubset(subscripts);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
