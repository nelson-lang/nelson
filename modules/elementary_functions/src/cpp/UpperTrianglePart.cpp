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
#include "UpperTrianglePart.hpp"
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
UpperTrianglePartSparseTemplate(const ArrayOf& A, signedIndexType offset)
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
            if ((signedIndexType)it.row() <= (signedIndexType)it.col() - offset) {
                triplets.emplace_back(it.row(), it.col(), it.value());
            }
        }
    }
    spmatDST->setFromTriplets(triplets.begin(), triplets.end());
    return makeSparseResult(A.dataClass(), A.dimensions(), std::move(spmatDST));
}
//=============================================================================
static ArrayOf
UpperTrianglePartSparse(const ArrayOf& A, signedIndexType offset, bool& needToOverload)
{
    needToOverload = false;
    switch (A.dataClass()) {
    case NLS_LOGICAL:
        return UpperTrianglePartSparseTemplate<logical>(A, offset);
    case NLS_SINGLE:
        return UpperTrianglePartSparseTemplate<single>(A, offset);
    case NLS_DOUBLE:
        return UpperTrianglePartSparseTemplate<double>(A, offset);
    case NLS_SCOMPLEX:
        return UpperTrianglePartSparseTemplate<singlecomplex>(A, offset);
    case NLS_DCOMPLEX:
        return UpperTrianglePartSparseTemplate<doublecomplex>(A, offset);
    default:
        needToOverload = true;
        return {};
    }
}
//=============================================================================
template <class T>
ArrayOf
UpperTrianglePartComplex(const ArrayOf& A, signedIndexType offset)
{
    NelsonType classA = A.dataClass();
    Dimensions dimsA = A.dimensions();
    indexType nbElements = dimsA.elementCount();
    T* D = (T*)ArrayOf::allocateArrayOf(classA, nbElements, stringVector(), true);
    auto* Dz = reinterpret_cast<std::complex<T>*>(D);
    T* S = (T*)A.dataPointer();
    auto* Sz = reinterpret_cast<std::complex<T>*>(S);
    indexType C = dimsA.columns();
    indexType R = dimsA.rows();
    OMP_PARALLEL_FOR_LOOP(C)
    for (ompIndexType i = 0; i < (ompIndexType)C; i++) {
        auto v = (signedIndexType)(i + 1 - offset);
        indexType iSize = std::min(std::max(v, (signedIndexType)0), (signedIndexType)R);
        memcpy(&Dz[i * R], &Sz[i * R], iSize * sizeof(std::complex<T>));
    }
    return ArrayOf(classA, dimsA, D);
}
//=============================================================================
template <class T>
ArrayOf
UpperTrianglePartReal(const ArrayOf& A, signedIndexType offset)
{
    NelsonType classA = A.dataClass();
    Dimensions dimsA = A.dimensions();
    indexType nbElements = dimsA.elementCount();
    T* D = (T*)ArrayOf::allocateArrayOf(classA, nbElements, stringVector(), true);
    T* S = (T*)A.dataPointer();
    indexType C = dimsA.columns();
    indexType R = dimsA.rows();
    OMP_PARALLEL_FOR_LOOP(C)
    for (ompIndexType i = 0; i < (ompIndexType)C; i++) {
        auto v = (signedIndexType)(i + 1 - offset);
        indexType iSize = std::min(std::max(v, (signedIndexType)0), (signedIndexType)R);
        memcpy(&D[i * R], &S[i * R], iSize * sizeof(T));
    }
    return ArrayOf(classA, dimsA, D);
}
//=============================================================================
ArrayOf
UpperTrianglePart(const ArrayOf& A, signedIndexType offset, bool& needToOverload)
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
        return UpperTrianglePartSparse(A, offset, needToOverload);
    }
    switch (A.dataClass()) {
    case NLS_LOGICAL: {
        res = UpperTrianglePartReal<logical>(A, offset);

    } break;
    case NLS_CHAR: {
        res = UpperTrianglePartReal<charType>(A, offset);
    } break;
    case NLS_UINT8: {
        res = UpperTrianglePartReal<uint8>(A, offset);
    } break;
    case NLS_UINT16: {
        res = UpperTrianglePartReal<uint16>(A, offset);
    } break;
    case NLS_UINT32: {
        res = UpperTrianglePartReal<uint32>(A, offset);
    } break;
    case NLS_UINT64: {
        res = UpperTrianglePartReal<uint64>(A, offset);
    } break;
    case NLS_INT8: {
        res = UpperTrianglePartReal<int8>(A, offset);
    } break;
    case NLS_INT16: {
        res = UpperTrianglePartReal<int16>(A, offset);
    } break;
    case NLS_INT32: {
        res = UpperTrianglePartReal<int32>(A, offset);
    } break;
    case NLS_INT64: {
        res = UpperTrianglePartReal<int64>(A, offset);
    } break;
    case NLS_SINGLE: {
        res = UpperTrianglePartReal<single>(A, offset);
    } break;
    case NLS_DOUBLE: {
        res = UpperTrianglePartReal<double>(A, offset);

    } break;
    case NLS_SCOMPLEX: {
        res = UpperTrianglePartComplex<single>(A, offset);
    } break;
    case NLS_DCOMPLEX: {
        res = UpperTrianglePartComplex<double>(A, offset);

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
