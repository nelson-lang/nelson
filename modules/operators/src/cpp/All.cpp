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
#include "All.hpp"
#include "Error.hpp"
#include "NelsonException.hpp"
#include "EmptyHelpers.hpp"
#include "SparseType.hpp"
#include "SparseArrayOf.hpp"
#include <Eigen/Sparse>
#include <vector>
//=============================================================================
namespace Nelson {
//=============================================================================
template <class T>
static ArrayOf
SparseAllTemplate(const ArrayOf& A, indexType dim, bool doOverAllElements)
{
    indexType rows = A.rows();
    indexType cols = A.columns();
    const auto* spmatSRC
        = static_cast<const Eigen::SparseMatrix<T, 0, signedIndexType>*>(A.sparseDataPointer());

    // all(vector) with the default dimension (and the 'all' option) reduces the
    // whole vector to a scalar. An explicit dimension must instead preserve shape
    // along a singleton dimension (e.g. all(rowVector, 1) returns the row
    // unchanged as logical), handled by the general per-dimension reduction below.
    if (doOverAllElements || (A.isVector() && dim == 0)) {
        indexType count = 0;
        for (indexType col = 0; col < (indexType)spmatSRC->outerSize(); ++col) {
            for (typename Eigen::SparseMatrix<T, 0, signedIndexType>::InnerIterator it(
                     *spmatSRC, col);
                it; ++it) {
                if (it.value() != T(0)) {
                    ++count;
                }
            }
        }
        return ArrayOf::logicalScalar(count == A.elementCount());
    }

    indexType workDim = (dim == 0 || dim == 1) ? 0 : dim - 1;
    Dimensions outDims = workDim == 0 ? Dimensions(1, cols) : Dimensions(rows, 1);
    std::vector<indexType> count(workDim == 0 ? cols : rows, 0);
    for (indexType col = 0; col < cols; ++col) {
        for (typename Eigen::SparseMatrix<T, 0, signedIndexType>::InnerIterator it(*spmatSRC, col);
            it; ++it) {
            if (it.value() != T(0)) {
                ++count[workDim == 0 ? col : it.row()];
            }
        }
    }

    auto* spmatDST
        = new Eigen::SparseMatrix<logical, 0, signedIndexType>(outDims.rows(), outDims.columns());
    indexType requiredCount = workDim == 0 ? rows : cols;
    for (indexType k = 0; k < (indexType)count.size(); ++k) {
        if (count[k] == requiredCount) {
            if (workDim == 0) {
                spmatDST->coeffRef(0, k) = 1;
            } else {
                spmatDST->coeffRef(k, 0) = 1;
            }
        }
    }
    indexType nz = spmatDST->nonZeros();
    ArrayOf res = makeSparseResult(NLS_LOGICAL, outDims, spmatDST);
    if (nz == 0) {
        res.setSparseNzmax(1);
    }
    return res;
}
//=============================================================================
static ArrayOf
SparseAll(const ArrayOf& A, indexType dim, bool doOverAllElements, bool& needToOverload)
{
    switch (A.dataClass()) {
    case NLS_LOGICAL:
        return SparseAllTemplate<logical>(A, dim, doOverAllElements);
    case NLS_SINGLE:
        return SparseAllTemplate<single>(A, dim, doOverAllElements);
    case NLS_DOUBLE:
        return SparseAllTemplate<double>(A, dim, doOverAllElements);
    case NLS_SCOMPLEX:
        return SparseAllTemplate<singlecomplex>(A, dim, doOverAllElements);
    case NLS_DCOMPLEX:
        return SparseAllTemplate<doublecomplex>(A, dim, doOverAllElements);
    default:
        needToOverload = true;
        return {};
    }
}
//=============================================================================
static ArrayOf
AllReduceND(const ArrayOf& A, indexType dim)
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
            bool acc = true;
            indexType base = p * planesize * linesize + e;
            for (indexType k = 0; k < linesize; k++) {
                if (in[base + k * planesize] == 0) {
                    acc = false;
                    break;
                }
            }
            out[p * planesize + e] = acc ? 1 : 0;
        }
    }
    return ArrayOf(NLS_LOGICAL, dimsRes, out);
}
//=============================================================================
ArrayOf
All(ArrayOf& A, indexType dim, bool doOverAllElements, bool& needToOverload)
{
    ArrayOf res;
    needToOverload = false;
    bool mustBeOverloaded = !(A.isNumeric() || A.isLogical() || A.isCharacterArray());
    if (mustBeOverloaded) {
        needToOverload = true;
        return {};
    }
    if (A.isSparse()) {
        return SparseAll(A, dim, doOverAllElements, needToOverload);
    }
    try {
        A.promoteType(NLS_LOGICAL);
    } catch (NelsonException&) {
        needToOverload = true;
        return {};
    }
    if (A.isEmpty()) {
        Dimensions dimsA = A.dimensions();
        Dimensions dimsRes = emptyDimensionsHelper(dimsA, dim);
        logical* logicalarray = (logical*)ArrayOf::allocateArrayOf(
            NLS_LOGICAL, dimsRes.elementCount(), stringVector(), false);
        memset(logicalarray, 1, dimsRes.elementCount());
        res = ArrayOf(NLS_LOGICAL, dimsRes, logicalarray);
    } else if (doOverAllElements) {
        bool bres = true;
        indexType elementCount = A.elementCount();
        auto* pLogical = (logical*)A.dataPointer();
        for (indexType k = 0; k < elementCount; k++) {
            if (!(pLogical[k] != 0)) {
                bres = false;
                break;
            }
        }
        res = ArrayOf::logicalScalar(bres);
    } else if (A.isVector()) {
        Dimensions dimsA = A.dimensions();
        bool isColumn = (dimsA.columns() == 1);
        // A singleton dimension is a no-op (all(col, 2) returns the column); the spanning
        // dimension collapses to a scalar.
        bool reduce = (dim == 0) || (isColumn ? (dim == 1) : (dim == 2));
        if (reduce) {
            auto* pLogical = (logical*)A.dataPointer();
            bool bRes = true;
            indexType elementCount = A.elementCount();
            for (indexType k = 0; k < elementCount; k++) {
                if (!(pLogical[k] != 0)) {
                    bRes = false;
                    break;
                }
            }
            res = ArrayOf::logicalScalar(bRes);
        } else {
            res = A;
            res.ensureSingleOwner();
        }
    } else if (!A.isEmpty() && !A.isVector() && !A.is2D()) {
        res = AllReduceND(A, dim);
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
            matC = matA.colwise().all().cast<logical>();
            res = ArrayOf(NLS_LOGICAL, Dimensions(1, nA), logicalarray);
        } else if (dim == 2) {
            logical* logicalarray = (logical*)ArrayOf::allocateArrayOf(NLS_LOGICAL, mA);
            Eigen::Map<Eigen::Matrix<logical, Eigen::Dynamic, Eigen::Dynamic>> matC(
                logicalarray, mA, 1);
            matC = matA.rowwise().all().cast<logical>();
            res = ArrayOf(NLS_LOGICAL, Dimensions(mA, 1), logicalarray);
        } else {
            res = A;
            res.ensureSingleOwner();
        }
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
