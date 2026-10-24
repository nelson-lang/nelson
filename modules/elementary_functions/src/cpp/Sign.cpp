//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "nlsBuildConfig.h"
#include "omp_for_loop.hpp"
#include "Sign.hpp"
#include "Decomplexify.hpp"
#include "SparseType.hpp"
#include "SparseArrayOf.hpp"
#include <cmath>
#include <memory>
#include <vector>
#include <Eigen/Sparse>
//=============================================================================
namespace Nelson {
//=============================================================================
template <class T>
ArrayOf
SignUnsignedInteger(NelsonType outputClass, const T* ptrA, const Dimensions& dimsA)
{
    ompIndexType nbElements = dimsA.elementCount();
    T* ptrB = (T*)ArrayOf::allocateArrayOf(outputClass, nbElements);
    OMP_PARALLEL_FOR_LOOP(nbElements)
    for (ompIndexType k = 0; k < nbElements; ++k) {
        if (ptrA[k] == (T)0) {
            ptrB[k] = (T)(0);
        } else {
            ptrB[k] = (T)(1);
        }
    }
    return ArrayOf(outputClass, dimsA, ptrB);
}
//=============================================================================
template <class T>
ArrayOf
SignSignedInteger(NelsonType outputClass, const T* ptrA, const Dimensions& dimsA)
{
    ompIndexType nbElements = dimsA.elementCount();
    T* ptrB = (T*)ArrayOf::allocateArrayOf(outputClass, nbElements);
    OMP_PARALLEL_FOR_LOOP(nbElements)
    for (ompIndexType k = 0; k < nbElements; ++k) {
        if (ptrA[k] < (T)0) {
            ptrB[k] = (T)(-1);
        } else if (ptrA[k] == (T)0) {
            ptrB[k] = (T)(0);
        } else {
            ptrB[k] = (T)(1);
        }
    }
    return ArrayOf(outputClass, dimsA, ptrB);
}
//=============================================================================
template <class T>
ArrayOf
SignReal(NelsonType outputClass, const T* ptrA, const Dimensions& dimsA)
{
    ompIndexType nbElements = dimsA.elementCount();
    T* ptrB = (T*)ArrayOf::allocateArrayOf(outputClass, nbElements);
    OMP_PARALLEL_FOR_LOOP(nbElements)
    for (ompIndexType k = 0; k < nbElements; ++k) {
        if (std::isnan(ptrA[k])) {
            ptrB[k] = (T)std::nan("");
        } else if (ptrA[k] < (T)0) {
            ptrB[k] = (T)(-1);
        } else if (ptrA[k] == (T)0) {
            ptrB[k] = (T)(0);
        } else {
            ptrB[k] = (T)(1);
        }
    }
    return ArrayOf(outputClass, dimsA, ptrB);
}
//=============================================================================
template <class T>
ArrayOf
SignComplex(NelsonType outputClass, const T* ptrA, const Dimensions& dimsA)
{
    ompIndexType nbElements = dimsA.elementCount();
    T* ptrB = (T*)ArrayOf::allocateArrayOf(outputClass, nbElements);
    OMP_PARALLEL_FOR_LOOP(nbElements)
    for (ompIndexType k = 0; k < nbElements * 2; k = k + 2) {
        std::complex<T> x(ptrA[k], ptrA[k + 1]);
        T magnitude = std::abs(x);
        if (magnitude == (T)0) {
            ptrB[k] = (T)0;
            ptrB[k + 1] = (T)0;
        } else {
            std::complex<T> r = x / magnitude;
            ptrB[k] = r.real();
            ptrB[k + 1] = r.imag();
        }
    }

    return decomplexify(ArrayOf(outputClass, dimsA, ptrB));
}
//=============================================================================
template <class T>
static ArrayOf
SparseSignReal(const ArrayOf& A, NelsonType outputClass)
{
    // Zeros stay zero and the result is real, so only the stored nonzeros are visited.
    indexType rows = A.rows();
    indexType cols = A.columns();
    const auto* spmatSRC
        = static_cast<const Eigen::SparseMatrix<T, 0, signedIndexType>*>(A.sparseDataPointer());
    auto spmatDST = std::make_unique<Eigen::SparseMatrix<T, 0, signedIndexType>>(rows, cols);
    std::vector<Eigen::Triplet<T, signedIndexType>> triplets;
    triplets.reserve(spmatSRC->nonZeros());
    for (signedIndexType k = 0; k < spmatSRC->outerSize(); ++k) {
        for (typename Eigen::SparseMatrix<T, 0, signedIndexType>::InnerIterator it(*spmatSRC, k);
            it; ++it) {
            T value = it.value();
            T signValue;
            if (std::isnan(value)) {
                signValue = (T)std::nan("");
            } else if (value < T(0)) {
                signValue = T(-1);
            } else if (value == T(0)) {
                signValue = T(0);
            } else {
                signValue = T(1);
            }
            if (signValue != T(0)) {
                triplets.emplace_back(it.row(), it.col(), signValue);
            }
        }
    }
    spmatDST->setFromTriplets(triplets.begin(), triplets.end());
    return makeSparseResult(outputClass, A.dimensions(), std::move(spmatDST));
}
//=============================================================================
ArrayOf
Sign(const ArrayOf& A, bool& needToOverload)
{
    if (A.isSparse()) {
        switch (A.dataClass()) {
        case NLS_DOUBLE:
            needToOverload = false;
            return SparseSignReal<double>(A, NLS_DOUBLE);
        case NLS_SINGLE:
            needToOverload = false;
            return SparseSignReal<single>(A, NLS_SINGLE);
        default: {
            // Complex and logical sparse: densify, compute, then make sparse again.
            ArrayOf dense(A);
            dense.makeDense();
            ArrayOf result = Sign(dense, needToOverload);
            if (!needToOverload) {
                result.makeSparse();
            }
            return result;
        }
        }
    }
    needToOverload = false;
    switch (A.dataClass()) {
    case NLS_LOGICAL:
    case NLS_CHAR: {
        ArrayOf promoted(A);
        promoted.promoteType(NLS_DOUBLE);
        Dimensions dimsA = A.dimensions();
        return SignReal<double>(NLS_DOUBLE, (double*)promoted.dataPointer(), dimsA);
    } break;
    case NLS_UINT8: {
        Dimensions dimsA = A.dimensions();
        return SignUnsignedInteger<uint8>(NLS_UINT8, (uint8*)A.dataPointer(), dimsA);
    } break;
    case NLS_INT8: {
        Dimensions dimsA = A.dimensions();
        return SignSignedInteger<int8>(NLS_INT8, (int8*)A.dataPointer(), dimsA);
    } break;
    case NLS_UINT16: {
        Dimensions dimsA = A.dimensions();
        return SignUnsignedInteger<uint16>(NLS_UINT16, (uint16*)A.dataPointer(), dimsA);
    } break;
    case NLS_INT16: {
        Dimensions dimsA = A.dimensions();
        return SignSignedInteger<int16>(NLS_INT16, (int16*)A.dataPointer(), dimsA);
    } break;
    case NLS_UINT32: {
        Dimensions dimsA = A.dimensions();
        return SignUnsignedInteger<uint32>(NLS_UINT32, (uint32*)A.dataPointer(), dimsA);
    } break;
    case NLS_INT32: {
        Dimensions dimsA = A.dimensions();
        return SignSignedInteger<int32>(NLS_INT32, (int32*)A.dataPointer(), dimsA);
    } break;
    case NLS_UINT64: {
        Dimensions dimsA = A.dimensions();
        return SignUnsignedInteger<uint64>(NLS_UINT64, (uint64*)A.dataPointer(), dimsA);
    } break;
    case NLS_INT64: {
        Dimensions dimsA = A.dimensions();
        return SignSignedInteger<int64>(NLS_INT64, (int64*)A.dataPointer(), dimsA);
    } break;
    case NLS_SINGLE: {
        Dimensions dimsA = A.dimensions();
        return SignReal<single>(NLS_SINGLE, (single*)A.dataPointer(), dimsA);
    } break;
    case NLS_DOUBLE: {
        Dimensions dimsA = A.dimensions();
        return SignReal<double>(NLS_DOUBLE, (double*)A.dataPointer(), dimsA);
    } break;
    case NLS_SCOMPLEX: {
        Dimensions dimsA = A.dimensions();
        return SignComplex<single>(NLS_SCOMPLEX, (single*)A.dataPointer(), dimsA);
    } break;
    case NLS_DCOMPLEX: {
        Dimensions dimsA = A.dimensions();
        return SignComplex<double>(NLS_DCOMPLEX, (double*)A.dataPointer(), dimsA);
    } break;
    case NLS_UNKNOWN:
    case NLS_GO_HANDLE:
    case NLS_HANDLE:
    case NLS_CELL_ARRAY:
    case NLS_STRUCT_ARRAY:
    case NLS_CLASS_ARRAY:
    case NLS_CLASSDEF_ARRAY:
    case NLS_FUNCTION_HANDLE:
    case NLS_STRING_ARRAY:
    default: {
        needToOverload = true;
    } break;
    }
    return {};
}
//=============================================================================
} // namespace Nelson
//=============================================================================
