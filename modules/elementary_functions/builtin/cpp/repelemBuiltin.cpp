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
#include "repelemBuiltin.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "OverloadRequired.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
static std::vector<indexType>
computeCounts(const ArrayOf& countArg, indexType len)
{
    std::vector<indexType> counts(len, 0);
    if (countArg.isScalar()) {
        double c = countArg.asDoubleScalar();
        if (!std::isfinite(c) || c < 0 || std::floor(c) != c) {
            Error(L"Nelson:elementary_functions:replicationCountsNonnegativeIntegers",
                L"Replication counts must be nonnegative integers.");
        }
        for (indexType i = 0; i < len; ++i) {
            counts[i] = (indexType)c;
        }
    } else {
        ArrayOf cc(countArg);
        cc.promoteType(NLS_DOUBLE);
        if (cc.elementCount() != len) {
            Error(L"Nelson:elementary_functions:replicationCountsMatchNumberElements",
                L"Replication counts must match the number of elements along the dimension.");
        }
        const double* p = (const double*)cc.dataPointer();
        for (indexType i = 0; i < len; ++i) {
            double c = p[i];
            if (!std::isfinite(c) || c < 0 || std::floor(c) != c) {
                Error(L"Nelson:elementary_functions:replicationCountsNonnegativeIntegers",
                    L"Replication counts must be nonnegative integers.");
            }
            counts[i] = (indexType)c;
        }
    }
    return counts;
}
//=============================================================================
static ArrayOf
countsToIndex(const std::vector<indexType>& counts)
{
    indexType total = 0;
    for (indexType c : counts) {
        total += c;
    }
    double* ptr = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, total);
    indexType pos = 0;
    for (size_t i = 0; i < counts.size(); ++i) {
        double value = (double)(i + 1);
        for (indexType k = 0; k < counts[i]; ++k) {
            ptr[pos++] = value;
        }
    }
    return ArrayOf(NLS_DOUBLE, Dimensions(1, total), ptr);
}
//=============================================================================
static ArrayOf
identityIndex(indexType len)
{
    double* ptr = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, len);
    for (indexType i = 0; i < len; ++i) {
        ptr[i] = (double)(i + 1);
    }
    return ArrayOf(NLS_DOUBLE, Dimensions(1, len), ptr);
}
//=============================================================================
// counts==nullptr means a uniform scalar count (scalarC) for all len elements.
template <class T>
static ArrayOf
repelemVectorTyped(const ArrayOf& A, NelsonType cls, const indexType* counts, indexType len,
    indexType scalarC, bool columnOut, int comps)
{
    indexType total = 0;
    if (counts == nullptr) {
        total = len * scalarC;
    } else {
        for (indexType i = 0; i < len; ++i) {
            total += counts[i];
        }
    }
    T* out = (T*)ArrayOf::allocateArrayOf(cls, total);
    const T* src = (const T*)A.dataPointer();
    indexType pos = 0;
    if (comps == 1) {
        for (indexType i = 0; i < len; ++i) {
            indexType c = (counts == nullptr) ? scalarC : counts[i];
            T value = src[i];
            for (indexType k = 0; k < c; ++k) {
                out[pos++] = value;
            }
        }
    } else {
        for (indexType i = 0; i < len; ++i) {
            indexType c = (counts == nullptr) ? scalarC : counts[i];
            for (indexType k = 0; k < c; ++k) {
                for (int comp = 0; comp < comps; ++comp) {
                    out[pos * comps + comp] = src[i * comps + comp];
                }
                ++pos;
            }
        }
    }
    Dimensions dims = columnOut ? Dimensions(total, 1) : Dimensions(1, total);
    return ArrayOf(cls, dims, out);
}
//=============================================================================
static bool
isFastCopyClass(NelsonType cls)
{
    switch (cls) {
    case NLS_LOGICAL:
    case NLS_UINT8:
    case NLS_INT8:
    case NLS_UINT16:
    case NLS_INT16:
    case NLS_UINT32:
    case NLS_INT32:
    case NLS_UINT64:
    case NLS_INT64:
    case NLS_SINGLE:
    case NLS_DOUBLE:
    case NLS_SCOMPLEX:
    case NLS_DCOMPLEX:
    case NLS_CHAR:
        return true;
    default:
        return false;
    }
}
//=============================================================================
static ArrayOf
repelemVectorFast(const ArrayOf& A, NelsonType cls, const indexType* counts, indexType len,
    indexType scalarC, bool columnOut)
{
    switch (cls) {
    case NLS_LOGICAL:
        return repelemVectorTyped<logical>(A, cls, counts, len, scalarC, columnOut, 1);
    case NLS_UINT8:
        return repelemVectorTyped<uint8>(A, cls, counts, len, scalarC, columnOut, 1);
    case NLS_INT8:
        return repelemVectorTyped<int8>(A, cls, counts, len, scalarC, columnOut, 1);
    case NLS_UINT16:
        return repelemVectorTyped<uint16>(A, cls, counts, len, scalarC, columnOut, 1);
    case NLS_INT16:
        return repelemVectorTyped<int16>(A, cls, counts, len, scalarC, columnOut, 1);
    case NLS_UINT32:
        return repelemVectorTyped<uint32>(A, cls, counts, len, scalarC, columnOut, 1);
    case NLS_INT32:
        return repelemVectorTyped<int32>(A, cls, counts, len, scalarC, columnOut, 1);
    case NLS_UINT64:
        return repelemVectorTyped<uint64>(A, cls, counts, len, scalarC, columnOut, 1);
    case NLS_INT64:
        return repelemVectorTyped<int64>(A, cls, counts, len, scalarC, columnOut, 1);
    case NLS_SINGLE:
        return repelemVectorTyped<single>(A, cls, counts, len, scalarC, columnOut, 1);
    case NLS_DOUBLE:
        return repelemVectorTyped<double>(A, cls, counts, len, scalarC, columnOut, 1);
    case NLS_CHAR:
        return repelemVectorTyped<charType>(A, cls, counts, len, scalarC, columnOut, 1);
    case NLS_SCOMPLEX:
        return repelemVectorTyped<single>(A, cls, counts, len, scalarC, columnOut, 2);
    case NLS_DCOMPLEX:
        return repelemVectorTyped<double>(A, cls, counts, len, scalarC, columnOut, 2);
    default:
        return {};
    }
}
//=============================================================================
ArrayOfVector
Nelson::ElementaryFunctionsGateway::repelemBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 2, -1);
    nargoutcheck(nLhs, 0, 1);
    ArrayOf A = argIn[0];
    if (A.isClassType() || A.isHandle()) {
        OverloadRequired("repelem");
    }
    Dimensions dims = A.dimensions();
    NelsonType cls = A.dataClass();
    if (argIn.size() == 2 && A.isVector() && isFastCopyClass(cls)) {
        indexType len = A.elementCount();
        bool columnOut = (dims[0] != 1);
        if (argIn[1].isScalar()) {
            double c = argIn[1].asDoubleScalar();
            if (!std::isfinite(c) || c < 0 || std::floor(c) != c) {
                Error(L"Nelson:elementary_functions:replicationCountsNonnegativeIntegers",
                    L"Replication counts must be nonnegative integers.");
            }
            retval << repelemVectorFast(A, cls, nullptr, len, (indexType)c, columnOut);
        } else {
            std::vector<indexType> counts = computeCounts(argIn[1], len);
            retval << repelemVectorFast(A, cls, counts.data(), len, 0, columnOut);
        }
        return retval;
    }
    ArrayOfVector indices;
    if (argIn.size() == 2 && A.isVector()) {
        indexType opdim = (dims[0] == 1) ? 1 : 0;
        for (indexType d = 0; d < 2; ++d) {
            if (d == opdim) {
                indices.push_back(countsToIndex(computeCounts(argIn[1], dims[d])));
            } else {
                indices.push_back(identityIndex(dims[d]));
            }
        }
    } else {
        size_t ndInput = dims.length();
        size_t nd = ndInput;
        if (argIn.size() - 1 > nd) {
            nd = argIn.size() - 1;
        }
        for (size_t d = 0; d < nd; ++d) {
            indexType len = (d < ndInput) ? dims[(indexType)d] : 1;
            if (d + 1 < argIn.size()) {
                indices.push_back(countsToIndex(computeCounts(argIn[d + 1], len)));
            } else {
                indices.push_back(identityIndex(len));
            }
        }
    }
    ArrayOf work = A;
    retval << work.nDimSubset(indices);
    return retval;
}
//=============================================================================
