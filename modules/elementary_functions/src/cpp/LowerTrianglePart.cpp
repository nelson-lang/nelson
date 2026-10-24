//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <cstring>
#include "nlsBuildConfig.h"
#include "omp_for_loop.hpp"
#include "LowerTrianglePart.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "SparseType.hpp"
#include "SparseArrayOf.hpp"
#include "i18n.hpp"
#include "Decomplexify.hpp"
#include <Eigen/Sparse>
#include <memory>
//=============================================================================
namespace Nelson {
//=============================================================================
template <class T>
static ArrayOf
LowerTrianglePartSparseTemplate(const ArrayOf& A, signedIndexType offset)
{
    const auto* spmatSRC
        = static_cast<const Eigen::SparseMatrix<T, 0, signedIndexType>*>(A.sparseDataPointer());
    auto spmatDST
        = std::make_unique<Eigen::SparseMatrix<T, 0, signedIndexType>>(A.rows(), A.columns());
    std::vector<Eigen::Triplet<T, signedIndexType>> triplets;
    triplets.reserve(spmatSRC->nonZeros());
    for (indexType k = 0; k < (indexType)spmatSRC->outerSize(); ++k) {
        for (typename Eigen::SparseMatrix<T, 0, signedIndexType>::InnerIterator it(*spmatSRC, k);
            it; ++it) {
            if ((signedIndexType)it.row() >= (signedIndexType)it.col() - offset) {
                triplets.emplace_back(it.row(), it.col(), it.value());
            }
        }
    }
    spmatDST->setFromTriplets(triplets.begin(), triplets.end());
    return makeSparseResult(A.dataClass(), A.dimensions(), std::move(spmatDST));
}
//=============================================================================
static ArrayOf
LowerTrianglePartSparse(const ArrayOf& A, signedIndexType offset, bool& needToOverload)
{
    needToOverload = false;
    switch (A.dataClass()) {
    case NLS_LOGICAL:
        return LowerTrianglePartSparseTemplate<logical>(A, offset);
    case NLS_SINGLE:
        return LowerTrianglePartSparseTemplate<single>(A, offset);
    case NLS_DOUBLE:
        return LowerTrianglePartSparseTemplate<double>(A, offset);
    case NLS_SCOMPLEX:
        return LowerTrianglePartSparseTemplate<singlecomplex>(A, offset);
    case NLS_DCOMPLEX:
        return LowerTrianglePartSparseTemplate<doublecomplex>(A, offset);
    default:
        needToOverload = true;
        return {};
    }
}
//=============================================================================
template <class T>
ArrayOf
LowerTrianglePartComplex(const ArrayOf& A, signedIndexType offset)
{
    Dimensions dimsA = A.dimensions();
    indexType nbElements = dimsA.elementCount();
    ArrayOf res = A;
    res.ensureSingleOwner();
    T* D = (T*)res.dataPointer();
    auto* Dz = reinterpret_cast<std::complex<T>*>(D);
    indexType C = dimsA.columns();
    indexType R = dimsA.rows();
    OMP_PARALLEL_FOR_LOOP(C)
    for (ompIndexType i = 0; i < (ompIndexType)C; i++) {
        auto v = (signedIndexType)(i - offset);
        indexType iSize = std::min(std::max(v, (signedIndexType)0), (signedIndexType)R);
        memset(&Dz[i * R], 0x00, iSize * sizeof(std::complex<T>));
    }
    return res;
}
//=============================================================================
template <class T>
ArrayOf
LowerTrianglePartReal(const ArrayOf& A, signedIndexType offset)
{
    Dimensions dimsA = A.dimensions();
    ArrayOf res = A;
    res.ensureSingleOwner();
    T* D = (T*)res.dataPointer();
    indexType C = dimsA.columns();
    indexType R = dimsA.rows();
    OMP_PARALLEL_FOR_LOOP(C)
    for (ompIndexType i = 0; i < (ompIndexType)C; i++) {
        auto v = (signedIndexType)(i - offset);
        indexType iSize = std::min(std::max(v, (signedIndexType)0), (signedIndexType)R);
        memset(&D[i * R], 0x00, iSize * sizeof(T));
    }
    return res;
}
//=============================================================================
ArrayOf
LowerTrianglePart(const ArrayOf& A, signedIndexType offset, bool& needToOverload)
{
    ArrayOf res;
    needToOverload = false;
    if (!(A.is2D() || A.isVector() || A.isEmpty())) {
        Error(L"Nelson:elementary_functions:vectorMatrix2DExpected",
            L"Vector or matrix 2D expected.");
    }
    if (A.isComplex() && A.isEmpty()) {
        // no elements: the result is an empty real array
        return decomplexify(A);
    }
    Dimensions dimsA = A.dimensions();
    if (A.isSparse()) {
        return LowerTrianglePartSparse(A, offset, needToOverload);
    }
    switch (A.dataClass()) {
    case NLS_LOGICAL: {
        res = LowerTrianglePartReal<logical>(A, offset);

    } break;
    case NLS_CHAR: {
        res = LowerTrianglePartReal<charType>(A, offset);
    } break;
    case NLS_UINT8: {
        res = LowerTrianglePartReal<uint8>(A, offset);
    } break;
    case NLS_UINT16: {
        res = LowerTrianglePartReal<uint16>(A, offset);
    } break;
    case NLS_UINT32: {
        res = LowerTrianglePartReal<uint32>(A, offset);
    } break;
    case NLS_UINT64: {
        res = LowerTrianglePartReal<uint64>(A, offset);
    } break;
    case NLS_INT8: {
        res = LowerTrianglePartReal<int8>(A, offset);
    } break;
    case NLS_INT16: {
        res = LowerTrianglePartReal<int16>(A, offset);
    } break;
    case NLS_INT32: {
        res = LowerTrianglePartReal<int32>(A, offset);
    } break;
    case NLS_INT64: {
        res = LowerTrianglePartReal<int64>(A, offset);
    } break;
    case NLS_SINGLE: {
        res = LowerTrianglePartReal<single>(A, offset);
    } break;
    case NLS_DOUBLE: {
        res = LowerTrianglePartReal<double>(A, offset);

    } break;
    case NLS_SCOMPLEX: {
        res = LowerTrianglePartComplex<single>(A, offset);
    } break;
    case NLS_DCOMPLEX: {
        res = LowerTrianglePartComplex<double>(A, offset);
    } break;
    default: {
        needToOverload = true;
        return {};
    } break;
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
