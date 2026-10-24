//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "CtransposeSparseDouble.hpp"
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
ctransposeSparseTyped(const ArrayOf& A, NelsonType dataClass)
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
    *spMatC = spMatA->adjoint();
    Dimensions dimsC = Dimensions(spMatC->rows(), spMatC->cols());
    return ArrayOf(dataClass, dimsC, (void*)spMatC, true);
}
//=============================================================================
ArrayOf
CtransposeSparseDouble(const ArrayOf& A)
{
    ArrayOf C;
    if (A.isEmpty()) {
        Dimensions dimsC(A.columns(), A.rows());
        C = ArrayOf(A.dataClass(), dimsC, (void*)nullptr, true);
    } else {
        switch (A.dataClass()) {
        case NLS_DOUBLE: {
            C = ctransposeSparseTyped<double>(A, NLS_DOUBLE);
        } break;
        case NLS_DCOMPLEX: {
            C = ctransposeSparseTyped<doublecomplex>(A, NLS_DCOMPLEX);
        } break;
        case NLS_SINGLE: {
            C = ctransposeSparseTyped<single>(A, NLS_SINGLE);
        } break;
        case NLS_SCOMPLEX: {
            C = ctransposeSparseTyped<singlecomplex>(A, NLS_SCOMPLEX);
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
