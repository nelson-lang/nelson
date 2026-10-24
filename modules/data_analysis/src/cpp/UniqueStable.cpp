//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "UniqueStable.hpp"
#include "Unique.hpp"
#include <algorithm>
#include <complex>
#include <numeric>
//=============================================================================
namespace Nelson {
//=============================================================================
template <class T>
static ArrayOf
reorderPlainArray(const ArrayOf& input, const std::vector<ompIndexType>& order, bool asRows)
{
    NelsonType cls = input.dataClass();
    Dimensions dimsOut(input.dimensions());
    T* op = static_cast<T*>(ArrayOf::allocateArrayOf(cls, input.elementCount()));
    const T* dp = static_cast<const T*>(input.dataPointer());
    if (asRows) {
        ompIndexType rows = input.rows();
        ompIndexType cols = input.columns();
        for (ompIndexType k = 0; k < (ompIndexType)order.size(); ++k) {
            for (ompIndexType j = 0; j < cols; ++j) {
                op[k + j * rows] = dp[order[k] + j * rows];
            }
        }
    } else {
        for (ompIndexType k = 0; k < (ompIndexType)order.size(); ++k) {
            op[k] = dp[order[k]];
        }
    }
    return ArrayOf(cls, dimsOut, op);
}
//=============================================================================
template <class T>
static ArrayOf
reorderComplexArray(const ArrayOf& input, const std::vector<ompIndexType>& order, bool asRows)
{
    NelsonType cls = input.dataClass();
    Dimensions dimsOut(input.dimensions());
    T* op = static_cast<T*>(ArrayOf::allocateArrayOf(cls, input.elementCount()));
    const T* dp = static_cast<const T*>(input.dataPointer());
    auto* opz = reinterpret_cast<std::complex<T>*>(op);
    auto* dpz = reinterpret_cast<const std::complex<T>*>(dp);
    if (asRows) {
        ompIndexType rows = input.rows();
        ompIndexType cols = input.columns();
        for (ompIndexType k = 0; k < (ompIndexType)order.size(); ++k) {
            for (ompIndexType j = 0; j < cols; ++j) {
                opz[k + j * rows] = dpz[order[k] + j * rows];
            }
        }
    } else {
        for (ompIndexType k = 0; k < (ompIndexType)order.size(); ++k) {
            opz[k] = dpz[order[k]];
        }
    }
    return ArrayOf(cls, dimsOut, op);
}
//=============================================================================
static ArrayOf
reorderArrayOfArrayOf(const ArrayOf& input, const std::vector<ompIndexType>& order, bool asRows)
{
    NelsonType cls = input.dataClass();
    Dimensions dimsOut(input.dimensions());
    ArrayOf* op = static_cast<ArrayOf*>(ArrayOf::allocateArrayOf(cls, input.elementCount()));
    const ArrayOf* dp = static_cast<const ArrayOf*>(input.dataPointer());
    if (asRows) {
        ompIndexType rows = input.rows();
        ompIndexType cols = input.columns();
        for (ompIndexType k = 0; k < (ompIndexType)order.size(); ++k) {
            for (ompIndexType j = 0; j < cols; ++j) {
                op[k + j * rows] = dp[order[k] + j * rows];
            }
        }
    } else {
        for (ompIndexType k = 0; k < (ompIndexType)order.size(); ++k) {
            op[k] = dp[order[k]];
        }
    }
    return ArrayOf(cls, dimsOut, op);
}
//=============================================================================
static ArrayOf
reorderUniqueValues(const ArrayOf& input, const std::vector<ompIndexType>& order, bool asRows)
{
    switch (input.dataClass()) {
    case NLS_DOUBLE:
        return reorderPlainArray<double>(input, order, asRows);
    case NLS_SINGLE:
        return reorderPlainArray<single>(input, order, asRows);
    case NLS_DCOMPLEX:
        return reorderComplexArray<double>(input, order, asRows);
    case NLS_SCOMPLEX:
        return reorderComplexArray<single>(input, order, asRows);
    case NLS_INT8:
        return reorderPlainArray<int8>(input, order, asRows);
    case NLS_INT16:
        return reorderPlainArray<int16>(input, order, asRows);
    case NLS_INT32:
        return reorderPlainArray<int32>(input, order, asRows);
    case NLS_INT64:
        return reorderPlainArray<int64>(input, order, asRows);
    case NLS_UINT8:
        return reorderPlainArray<uint8>(input, order, asRows);
    case NLS_UINT16:
        return reorderPlainArray<uint16>(input, order, asRows);
    case NLS_UINT32:
        return reorderPlainArray<uint32>(input, order, asRows);
    case NLS_UINT64:
        return reorderPlainArray<uint64>(input, order, asRows);
    case NLS_LOGICAL:
        return reorderPlainArray<logical>(input, order, asRows);
    case NLS_CHAR:
        return reorderPlainArray<charType>(input, order, asRows);
    case NLS_CELL_ARRAY:
    case NLS_STRING_ARRAY:
        return reorderArrayOfArrayOf(input, order, asRows);
    default:
        return input;
    }
}
//=============================================================================
static ArrayOf
reorderIndexVector(const ArrayOf& input, const std::vector<ompIndexType>& order)
{
    double* op = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, order.size()));
    const double* dp = static_cast<const double*>(input.dataPointer());
    for (ompIndexType k = 0; k < (ompIndexType)order.size(); ++k) {
        op[k] = dp[order[k]];
    }
    return ArrayOf(NLS_DOUBLE, Dimensions(order.size(), 1), op);
}
//=============================================================================
ArrayOfVector
UniqueStable(const ArrayOf& input, int nLhs, bool asRows, bool& needOverload)
{
    ArrayOfVector sorted = Unique(input, 3, asRows, needOverload, false);
    if (needOverload) {
        return {};
    }
    const double* ia = static_cast<const double*>(sorted[1].dataPointer());
    const double* ic = static_cast<const double*>(sorted[2].dataPointer());
    ompIndexType uniqueCount = sorted[1].elementCount();
    ompIndexType originalCount = sorted[2].elementCount();
    std::vector<ompIndexType> order(uniqueCount);
    std::iota(order.begin(), order.end(), 0);
    std::stable_sort(
        order.begin(), order.end(), [ia](ompIndexType a, ompIndexType b) { return ia[a] < ia[b]; });
    std::vector<ompIndexType> oldToNew(uniqueCount);
    for (ompIndexType k = 0; k < uniqueCount; ++k) {
        oldToNew[order[k]] = k + 1;
    }
    ArrayOfVector retval;
    retval << reorderUniqueValues(sorted[0], order, asRows);
    if (nLhs >= 2) {
        retval << reorderIndexVector(sorted[1], order);
    }
    if (nLhs >= 3) {
        double* op = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, originalCount));
        for (ompIndexType k = 0; k < originalCount; ++k) {
            op[k] = (double)oldToNew[(ompIndexType)ic[k] - 1];
        }
        retval << ArrayOf(NLS_DOUBLE, Dimensions(originalCount, 1), op);
    }
    return retval;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
