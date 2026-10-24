//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "lapack_eigen_config.hpp"
#include <Eigen/Dense>
#include "Any.hpp"
#include "Error.hpp"
#include "NelsonException.hpp"
#include "EmptyHelpers.hpp"
#include "SparseType.hpp"
#include "SparseArrayOf.hpp"
#include <Eigen/Sparse>
#include <cmath>
#include <vector>
//=============================================================================
namespace Nelson {
//=============================================================================
// 'any' truth test for a single element: an element counts as true iff it is
// nonzero and not NaN. For complex values, a NaN in either component makes the
// element non-true, regardless of the other component.
static inline bool
anyElementIsTrue(logical v)
{
    return v != 0;
}
//=============================================================================
static inline bool
anyElementIsTrue(double v)
{
    return v != 0.0 && !std::isnan(v);
}
//=============================================================================
static inline bool
anyElementIsTrue(single v)
{
    return v != 0.0f && !std::isnan(v);
}
//=============================================================================
static inline bool
anyElementIsTrue(const doublecomplex& v)
{
    return !std::isnan(v.real()) && !std::isnan(v.imag()) && (v.real() != 0.0 || v.imag() != 0.0);
}
//=============================================================================
static inline bool
anyElementIsTrue(const singlecomplex& v)
{
    return !std::isnan(v.real()) && !std::isnan(v.imag()) && (v.real() != 0.0f || v.imag() != 0.0f);
}
//=============================================================================
template <class T>
static ArrayOf
SparseAnyTemplate(const ArrayOf& A, indexType dim)
{
    indexType rows = A.rows();
    indexType cols = A.columns();
    const auto* spmatSRC
        = static_cast<const Eigen::SparseMatrix<T, 0, signedIndexType>*>(A.sparseDataPointer());

    // any(vector) with the default dimension reduces the whole vector to a
    // scalar. An explicit dimension must instead preserve shape along a singleton
    // dimension (e.g. any(rowVector, 1) returns the row unchanged as logical), so
    // it is handled by the general per-dimension reduction below.
    if (A.isVector() && dim == 0) {
        bool found = false;
        for (indexType col = 0; col < (indexType)spmatSRC->outerSize() && !found; ++col) {
            for (typename Eigen::SparseMatrix<T, 0, signedIndexType>::InnerIterator it(
                     *spmatSRC, col);
                it; ++it) {
                if (anyElementIsTrue(it.value())) {
                    found = true;
                    break;
                }
            }
        }
        return ArrayOf::logicalScalar(found);
    }

    indexType workDim = (dim == 0 || dim == 1) ? 0 : dim - 1;
    Dimensions outDims = workDim == 0 ? Dimensions(1, cols) : Dimensions(rows, 1);
    std::vector<logical> found(workDim == 0 ? cols : rows, 0);
    for (indexType col = 0; col < cols; ++col) {
        for (typename Eigen::SparseMatrix<T, 0, signedIndexType>::InnerIterator it(*spmatSRC, col);
            it; ++it) {
            if (anyElementIsTrue(it.value())) {
                found[workDim == 0 ? col : it.row()] = 1;
            }
        }
    }

    auto* spmatDST
        = new Eigen::SparseMatrix<logical, 0, signedIndexType>(outDims.rows(), outDims.columns());
    for (indexType k = 0; k < (indexType)found.size(); ++k) {
        if (found[k]) {
            if (workDim == 0) {
                spmatDST->coeffRef(0, k) = 1;
            } else {
                spmatDST->coeffRef(k, 0) = 1;
            }
        }
    }
    return makeSparseResult(NLS_LOGICAL, outDims, spmatDST);
}
//=============================================================================
static ArrayOf
SparseAny(const ArrayOf& A, indexType dim, bool& needToOverload)
{
    switch (A.dataClass()) {
    case NLS_LOGICAL:
        return SparseAnyTemplate<logical>(A, dim);
    case NLS_SINGLE:
        return SparseAnyTemplate<single>(A, dim);
    case NLS_DOUBLE:
        return SparseAnyTemplate<double>(A, dim);
    case NLS_SCOMPLEX:
        return SparseAnyTemplate<singlecomplex>(A, dim);
    case NLS_DCOMPLEX:
        return SparseAnyTemplate<doublecomplex>(A, dim);
    default:
        needToOverload = true;
        return {};
    }
}
//=============================================================================
template <class T>
static ArrayOf
SparseAnyAllTemplate(const ArrayOf& A)
{
    const auto* spmatSRC
        = static_cast<const Eigen::SparseMatrix<T, 0, signedIndexType>*>(A.sparseDataPointer());
    for (indexType col = 0; col < (indexType)spmatSRC->outerSize(); ++col) {
        for (typename Eigen::SparseMatrix<T, 0, signedIndexType>::InnerIterator it(*spmatSRC, col);
            it; ++it) {
            if (anyElementIsTrue(it.value())) {
                return ArrayOf::logicalScalar(true);
            }
        }
    }
    return ArrayOf::logicalScalar(false);
}
//=============================================================================
static ArrayOf
AnyReduceND(const ArrayOf& A, indexType dim)
{
    Dimensions dimsA = A.dimensions();
    indexType workDim;
    if (dim == 0) {
        indexType l = 0;
        while (l < (indexType)dimsA.length() && dimsA[l] == 1) {
            l++;
        }
        workDim = (l >= (indexType)dimsA.length()) ? 0 : l;
    } else {
        workDim = dim - 1;
    }
    if (workDim >= (indexType)dimsA.length() || dimsA[workDim] == 1) {
        ArrayOf res = A;
        res.ensureSingleOwner();
        return res;
    }
    indexType linesize = dimsA[workDim];
    indexType planesize = 1;
    for (indexType l = 0; l < workDim; l++) {
        planesize *= dimsA[l];
    }
    indexType planecount = 1;
    for (indexType l = workDim + 1; l < (indexType)dimsA.length(); l++) {
        planecount *= dimsA[l];
    }
    Dimensions dimsRes = dimsA;
    dimsRes.setDimensionLength(workDim, 1);
    dimsRes.simplify();
    logical* out = (logical*)ArrayOf::allocateArrayOf(NLS_LOGICAL, dimsRes.elementCount());
    const logical* in = (const logical*)A.dataPointer();
    for (indexType p = 0; p < planecount; p++) {
        for (indexType e = 0; e < planesize; e++) {
            bool acc = false;
            indexType base = p * planesize * linesize + e;
            for (indexType k = 0; k < linesize; k++) {
                if (in[base + k * planesize] != 0) {
                    acc = true;
                    break;
                }
            }
            out[p * planesize + e] = acc ? 1 : 0;
        }
    }
    return ArrayOf(NLS_LOGICAL, dimsRes, out);
}
//=============================================================================
// Converts A in place to a logical array using the 'any' truth semantics: a
// floating element counts as true iff it is nonzero and not NaN (for complex,
// a NaN in either component makes the element false). Non-floating inputs
// (logical, integer, char) use the standard logical promotion, which may throw
// for unsupported classes so the caller can request an overload.
template <class T>
static void
promoteFloatToLogicalForAny(ArrayOf& A)
{
    indexType n = A.elementCount();
    logical* dst = (logical*)ArrayOf::allocateArrayOf(NLS_LOGICAL, n, stringVector(), false);
    const T* src = (const T*)A.dataPointer();
    for (indexType k = 0; k < n; ++k) {
        dst[k] = anyElementIsTrue(src[k]) ? 1 : 0;
    }
    Dimensions dims = A.dimensions();
    A = ArrayOf(NLS_LOGICAL, dims, dst);
}
//=============================================================================
static void
promoteToLogicalForAny(ArrayOf& A)
{
    switch (A.dataClass()) {
    case NLS_DOUBLE:
        promoteFloatToLogicalForAny<double>(A);
        break;
    case NLS_SINGLE:
        promoteFloatToLogicalForAny<single>(A);
        break;
    case NLS_DCOMPLEX:
        promoteFloatToLogicalForAny<doublecomplex>(A);
        break;
    case NLS_SCOMPLEX:
        promoteFloatToLogicalForAny<singlecomplex>(A);
        break;
    default:
        A.promoteType(NLS_LOGICAL);
        break;
    }
}
//=============================================================================
ArrayOf
Any(ArrayOf& A, indexType dim, bool& needToOverload)
{
    ArrayOf res;
    needToOverload = false;
    if (A.isSparse()) {
        return SparseAny(A, dim, needToOverload);
    }
    try {
        promoteToLogicalForAny(A);
    } catch (NelsonException&) {
        needToOverload = true;
        return {};
    }
    if (A.isEmpty()) {
        Dimensions dimsA = A.dimensions();
        Dimensions dimsRes = emptyDimensionsHelper(dimsA, dim);
        logical* logicalarray = (logical*)ArrayOf::allocateArrayOf(
            NLS_LOGICAL, dimsRes.elementCount(), stringVector(), true);
        res = ArrayOf(NLS_LOGICAL, dimsRes, logicalarray);
    } else if (A.isVector()) {
        Dimensions dimsA = A.dimensions();
        bool isColumn = (dimsA.columns() == 1);
        // A singleton dimension is a no-op (any(col, 2) returns the column); the spanning
        // dimension collapses to a scalar.
        bool reduce = (dim == 0) || (isColumn ? (dim == 1) : (dim == 2));
        if (reduce) {
            auto* pLogical = (logical*)A.dataPointer();
            bool bRes = false;
            indexType elementCount = A.elementCount();
            for (indexType k = 0; k < elementCount; k++) {
                if (pLogical[k] != 0) {
                    bRes = true;
                    break;
                }
            }
            res = ArrayOf::logicalScalar(bRes);
        } else {
            res = A;
            res.ensureSingleOwner();
        }
    } else if (!A.isEmpty() && !A.isVector() && !A.is2D()) {
        res = AnyReduceND(A, dim);
    } else if (!A.isEmpty() && !A.isVector()) {
        Dimensions dims = A.dimensions();
        indexType mA = dims.rows();
        indexType nA = dims.columns();
        Eigen::Map<Eigen::Matrix<logical, Eigen::Dynamic, Eigen::Dynamic>> matA(
            (logical*)A.dataPointer(), mA, nA);
        if ((dim == 0) || (dim == 1)) {
            logical* logicalarray
                = (logical*)ArrayOf::allocateArrayOf(NLS_LOGICAL, nA, stringVector(), false);
            Eigen::Map<Eigen::Matrix<logical, Eigen::Dynamic, Eigen::Dynamic>> matC(
                logicalarray, 1, nA);
            matC = matA.colwise().any().cast<logical>();
            res = ArrayOf(NLS_LOGICAL, Dimensions(1, nA), logicalarray);
        } else if (dim == 2) {
            logical* logicalarray
                = (logical*)ArrayOf::allocateArrayOf(NLS_LOGICAL, mA, stringVector(), false);
            Eigen::Map<Eigen::Matrix<logical, Eigen::Dynamic, Eigen::Dynamic>> matC(
                logicalarray, mA, 1);
            matC = matA.rowwise().any().cast<logical>();
            res = ArrayOf(NLS_LOGICAL, Dimensions(mA, 1), logicalarray);
        } else {
            res = A;
            res.ensureSingleOwner();
        }
    }
    return res;
}
//=============================================================================
ArrayOf
AnyAll(ArrayOf& A, bool& needToOverload)
{
    ArrayOf res;
    needToOverload = false;
    if (A.isSparse()) {
        switch (A.dataClass()) {
        case NLS_LOGICAL:
            return SparseAnyAllTemplate<logical>(A);
        case NLS_SINGLE:
            return SparseAnyAllTemplate<single>(A);
        case NLS_DOUBLE:
            return SparseAnyAllTemplate<double>(A);
        case NLS_SCOMPLEX:
            return SparseAnyAllTemplate<singlecomplex>(A);
        case NLS_DCOMPLEX:
            return SparseAnyAllTemplate<doublecomplex>(A);
        default:
            needToOverload = true;
            return {};
        }
    }
    try {
        promoteToLogicalForAny(A);
    } catch (NelsonException&) {
        needToOverload = true;
        return {};
    }
    if (A.isEmpty()) {
        Dimensions dimsRes(1, 1);
        logical* logicalarray = (logical*)ArrayOf::allocateArrayOf(
            NLS_LOGICAL, dimsRes.elementCount(), stringVector(), true);
        return ArrayOf(NLS_LOGICAL, dimsRes, logicalarray);
    }
    auto* pLogical = (logical*)A.dataPointer();
    bool bRes = false;
    indexType elementCount = A.elementCount();
    for (indexType k = 0; k < elementCount; k++) {
        if (pLogical[k] != 0) {
            bRes = true;
            break;
        }
    }
    return ArrayOf::logicalScalar(bRes);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
