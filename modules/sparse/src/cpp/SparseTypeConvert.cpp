//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#define _SCL_SECURE_NO_WARNINGS
//=============================================================================
#include "lapack_eigen_config.hpp"
#include <cmath>
#include <type_traits>
#include <Eigen/Dense>
#include <Eigen/Sparse>
#include "SparseType.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "NewWithException.hpp"
//=============================================================================
// Class conversion of sparse matrices.
//=============================================================================
template <class InputType, class OutputType>
void*
Eigen_CastSparseMatrix(const void* cp)
{
    const auto* spMat = static_cast<const Eigen::SparseMatrix<InputType, 0, signedIndexType>*>(cp);
    Eigen::SparseMatrix<OutputType, 0, signedIndexType>* spMatdest = nullptr;
    try {
        spMatdest = new Eigen::SparseMatrix<OutputType, 0, signedIndexType>(
            spMat->template cast<OutputType>());
    } catch (const std::bad_alloc&) {
        Error(L"Nelson:outOfMemory", L"Memory allocation error... You may have run out of memory!");
    }
    spMatdest->finalize();
    spMatdest->makeCompressed();
    return (void*)spMatdest;
}
//=============================================================================
template <class InputType, class OutputType>
void*
Eigen_RealPartSparseMatrix(const void* cp)
{
    const auto* spMat = static_cast<const Eigen::SparseMatrix<InputType, 0, signedIndexType>*>(cp);
    Eigen::SparseMatrix<OutputType, 0, signedIndexType>* spMatdest = nullptr;
    try {
        spMatdest = new Eigen::SparseMatrix<OutputType, 0, signedIndexType>(
            spMat->real().template cast<OutputType>());
    } catch (const std::bad_alloc&) {
        Error(L"Nelson:outOfMemory", L"Memory allocation error... You may have run out of memory!");
    }
    // A purely imaginary entry has a zero real part: it is no longer stored.
    spMatdest->prune([](const signedIndexType&, const signedIndexType&, const OutputType& value) {
        return value != OutputType(0);
    });
    spMatdest->makeCompressed();
    return (void*)spMatdest;
}
//=============================================================================
template <class InputType>
void*
Eigen_SparseMatrixToLogical(indexType rows, indexType cols, const void* cp)
{
    const auto* spMat = static_cast<const Eigen::SparseMatrix<InputType, 0, signedIndexType>*>(cp);
    Eigen::SparseMatrix<logical, 0, signedIndexType>* spMatdest = nullptr;
    try {
        spMatdest = new Eigen::SparseMatrix<logical, 0, signedIndexType>(rows, cols);
    } catch (const std::bad_alloc&) {
        Error(L"Nelson:outOfMemory", L"Memory allocation error... You may have run out of memory!");
    }
    if (rows * cols) {
        std::vector<Eigen::Triplet<logical, signedIndexType>> triplets;
        triplets.reserve(spMat->nonZeros());
        for (indexType k = 0; k < (indexType)spMat->outerSize(); ++k) {
            for (typename Eigen::SparseMatrix<InputType, 0, signedIndexType>::InnerIterator it(
                     *spMat, k);
                it; ++it) {
                if constexpr (std::is_floating_point<InputType>::value) {
                    if (std::isnan(static_cast<double>(it.value()))) {
                        delete spMatdest;
                        Error(L"Nelson:logical:nanNotAllowed",
                            L"Conversion to logical with NaN is not possible.");
                    }
                }
                if (it.value() != InputType(0)) {
                    triplets.emplace_back(it.row(), it.col(), (logical)1);
                }
            }
        }
        // setFromTriplets builds a transposed intermediate sized by the row count:
        // for an empty result it allocates O(rows) for nothing (bad_alloc on a huge
        // all-zero sparse). The destination is already the empty matrix, so skip it.
        if (!triplets.empty()) {
            spMatdest->setFromTriplets(triplets.begin(), triplets.end());
        }
    }
    spMatdest->makeCompressed();
    return (void*)spMatdest;
}
//=============================================================================
void*
Eigen_TypeConvertSparse(
    NelsonType dclass, indexType rows, indexType cols, const void* cp, NelsonType oclass)
{
    switch (oclass) {
    case NLS_LOGICAL: {
        Eigen::SparseMatrix<logical, 0, signedIndexType>* spMat
            = (Eigen::SparseMatrix<logical, 0, signedIndexType>*)cp;
        switch (dclass) {
        case NLS_LOGICAL: {
            return Eigen_CopySparseMatrix(NLS_LOGICAL, rows, cols, cp);
        } break;
        case NLS_DOUBLE: {
            Eigen::SparseMatrix<double, 0, signedIndexType>* spMatdest = nullptr;
            try {
                spMatdest
                    = new Eigen::SparseMatrix<double, 0, signedIndexType>(spMat->cast<double>());
            } catch (const std::bad_alloc&) {
                Error(L"Nelson:outOfMemory",
                    L"Memory allocation error... You may have run out of memory!");
            }
            spMatdest->finalize();
            spMatdest->makeCompressed();
            return (void*)spMatdest;
        } break;
        case NLS_SINGLE: {
            return Eigen_CastSparseMatrix<logical, single>(cp);
        } break;
        case NLS_DCOMPLEX: {
            Eigen::SparseMatrix<doublecomplex, 0, signedIndexType>* spMatdest = nullptr;
            try {
                spMatdest = new Eigen::SparseMatrix<doublecomplex, 0, signedIndexType>(
                    spMat->cast<doublecomplex>());
            } catch (const std::bad_alloc&) {
                Error(L"Nelson:outOfMemory",
                    L"Memory allocation error... You may have run out of memory!");
            }
            spMatdest->finalize();
            spMatdest->makeCompressed();
            return (void*)spMatdest;
        } break;
        case NLS_SCOMPLEX: {
            return Eigen_CastSparseMatrix<logical, singlecomplex>(cp);
        } break;
        default: {
            Error(L"Nelson:sparse:typeNotSupported", L"Unsupported type in %s.",
                L"TypeConvertSparse");
        } break;
        }
    } break;
    case NLS_DOUBLE: {
        Eigen::SparseMatrix<double, 0, signedIndexType>* spMat
            = (Eigen::SparseMatrix<double, 0, signedIndexType>*)cp;
        switch (dclass) {
        case NLS_LOGICAL: {
            return Eigen_SparseMatrixToLogical<double>(rows, cols, cp);
        } break;
        case NLS_SINGLE: {
            return Eigen_CastSparseMatrix<double, single>(cp);
        } break;
        case NLS_DOUBLE: {
            return Eigen_CopySparseMatrix(NLS_DOUBLE, rows, cols, cp);
        } break;
        case NLS_DCOMPLEX: {
            Eigen::SparseMatrix<doublecomplex, 0, signedIndexType>* spMatdest = nullptr;
            try {
                spMatdest = new Eigen::SparseMatrix<doublecomplex, 0, signedIndexType>(rows, cols);
            } catch (const std::bad_alloc&) {
                Error(L"Nelson:outOfMemory",
                    L"Memory allocation error... You may have run out of memory!");
            }
            if (rows * cols) {
                std::vector<Eigen::Triplet<doublecomplex, signedIndexType>> triplets;
                triplets.reserve(spMat->nonZeros());
                for (indexType k = 0; k < (indexType)spMat->outerSize(); ++k) {
                    for (Eigen::SparseMatrix<double, 0, signedIndexType>::InnerIterator it(
                             *spMat, k);
                        it; ++it) {
                        triplets.emplace_back(it.row(), it.col(), doublecomplex(it.value(), 0.0));
                    }
                }
                spMatdest->setFromTriplets(triplets.begin(), triplets.end());
            }
            spMatdest->makeCompressed();
            return (void*)spMatdest;
        } break;
        case NLS_SCOMPLEX: {
            return Eigen_CastSparseMatrix<double, singlecomplex>(cp);
        } break;
        default: {
            Error(L"Nelson:sparse:typeNotSupported", L"Unsupported type in %s.",
                L"TypeConvertSparse");
        } break;
        }
    } break;
    case NLS_SINGLE: {
        switch (dclass) {
        case NLS_LOGICAL: {
            return Eigen_SparseMatrixToLogical<single>(rows, cols, cp);
        } break;
        case NLS_SINGLE: {
            return Eigen_CopySparseMatrix(NLS_SINGLE, rows, cols, cp);
        } break;
        case NLS_DOUBLE: {
            return Eigen_CastSparseMatrix<single, double>(cp);
        } break;
        case NLS_SCOMPLEX: {
            return Eigen_CastSparseMatrix<single, singlecomplex>(cp);
        } break;
        case NLS_DCOMPLEX: {
            return Eigen_CastSparseMatrix<single, doublecomplex>(cp);
        } break;
        default: {
            Error(L"Nelson:sparse:typeNotSupported", L"Unsupported type in %s.",
                L"TypeConvertSparse");
        } break;
        }
    } break;
    case NLS_DCOMPLEX: {
        switch (dclass) {
        case NLS_LOGICAL: {
            Error(L"Nelson:sparse:typeNotSupported", L"Unsupported type in %s.",
                L"TypeConvertSparse (complex to logical)");
        } break;
        case NLS_DOUBLE: {
            return Eigen_RealPartSparseMatrix<doublecomplex, double>(cp);
        } break;
        case NLS_SINGLE: {
            return Eigen_RealPartSparseMatrix<doublecomplex, single>(cp);
        } break;
        case NLS_DCOMPLEX: {
            return Eigen_CopySparseMatrix(NLS_DCOMPLEX, rows, cols, cp);
        } break;
        case NLS_SCOMPLEX: {
            return Eigen_CastSparseMatrix<doublecomplex, singlecomplex>(cp);
        } break;
        default: {
            Error(L"Nelson:sparse:typeNotSupported", L"Unsupported type in %s.",
                L"TypeConvertSparse");
        } break;
        }
    } break;
    case NLS_SCOMPLEX: {
        switch (dclass) {
        case NLS_LOGICAL: {
            Error(L"Nelson:sparse:typeNotSupported", L"Unsupported type in %s.",
                L"TypeConvertSparse (complex to logical)");
        } break;
        case NLS_DOUBLE: {
            return Eigen_RealPartSparseMatrix<singlecomplex, double>(cp);
        } break;
        case NLS_SINGLE: {
            return Eigen_RealPartSparseMatrix<singlecomplex, single>(cp);
        } break;
        case NLS_DCOMPLEX: {
            return Eigen_CastSparseMatrix<singlecomplex, doublecomplex>(cp);
        } break;
        case NLS_SCOMPLEX: {
            return Eigen_CopySparseMatrix(NLS_SCOMPLEX, rows, cols, cp);
        } break;
        default: {
            Error(L"Nelson:sparse:typeNotSupported", L"Unsupported type in %s.",
                L"TypeConvertSparse");
        } break;
        }
    } break;
    default: {
        Error(L"Nelson:sparse:typeNotSupported", L"Unsupported type in %s.", L"TypeConvertSparse");
    } break;
    }
    return nullptr;
}
//=============================================================================
