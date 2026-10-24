//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "TransposeSparseDouble.hpp"
#include "lapack_eigen_config.hpp"
#include <Eigen/Dense>
#include <Eigen/Sparse>
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
template <class T>
ArrayOf
transposeSparseTyped(const ArrayOf& A, NelsonType dataClass)
{
    const auto* spMatA
        = static_cast<const Eigen::SparseMatrix<T, 0, signedIndexType>*>(A.sparseDataPointer());
    Eigen::SparseMatrix<T, 0, signedIndexType>* spMatC;
    try {
        spMatC = new Eigen::SparseMatrix<T, 0, signedIndexType>(spMatA->cols(), spMatA->rows());
    } catch (const std::bad_alloc&) {
        spMatC = nullptr;
        Error(L"Nelson:outOfMemory", L"Memory allocation error... You may have run out of memory!");
    }
    *spMatC = spMatA->transpose();
    Dimensions dimsC = Dimensions(spMatC->rows(), spMatC->cols());
    return ArrayOf(dataClass, dimsC, (void*)spMatC, true);
}
//=============================================================================
ArrayOf
TransposeSparseDouble(const ArrayOf& A)
{
    ArrayOf C;
    if (A.isEmpty()) {
        Dimensions dimsC(A.columns(), A.rows());
        C = ArrayOf(A.dataClass(), dimsC, (void*)nullptr, true);
    } else {
        switch (A.dataClass()) {
        case NLS_DOUBLE: {
            C = transposeSparseTyped<double>(A, NLS_DOUBLE);
        } break;
        case NLS_DCOMPLEX: {
            C = transposeSparseTyped<doublecomplex>(A, NLS_DCOMPLEX);
        } break;
        case NLS_SINGLE: {
            C = transposeSparseTyped<single>(A, NLS_SINGLE);
        } break;
        case NLS_SCOMPLEX: {
            C = transposeSparseTyped<singlecomplex>(A, NLS_SCOMPLEX);
        } break;
        default: {
            Error(L"Nelson:types:sparseFloatingPointExpected",
                L"Sparse floating-point matrix expected.");
        } break;
        }
    }
    return C;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
