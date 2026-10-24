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
#include "UminusSparse.hpp"
#include "ClassName.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "SparseType.hpp"
#include "lapack_eigen_config.hpp"
#include <Eigen/Dense>
#include <Eigen/Sparse>
//=============================================================================
namespace Nelson {
//=============================================================================
template <class T>
void*
sparsedouble_uminus(NelsonType dclass, indexType rows, indexType cols, const void* cp)
{
    Eigen::SparseMatrix<T, 0, signedIndexType>* spMat
        = (Eigen::SparseMatrix<T, 0, signedIndexType>*)cp;
    Eigen::SparseMatrix<T, 0, signedIndexType>* res
        = (Eigen::SparseMatrix<T, 0, signedIndexType>*)Eigen_CopySparseMatrix(
            dclass, rows, cols, cp);
    for (indexType k = 0; k < (indexType)spMat->outerSize(); ++k) {
        for (typename Eigen::SparseMatrix<T, 0, signedIndexType>::InnerIterator it(*spMat, k); it;
            ++it) {
            res->coeffRef(it.row(), it.col()) = -it.value();
        }
    }
    res->finalize();
    res->makeCompressed();
    return (void*)res;
}
//=============================================================================
ArrayOf
sparsedouble_uminus(const ArrayOf& a)
{
    switch (a.dataClass()) {
    case NLS_DOUBLE: {
        void* res
            = sparsedouble_uminus<double>(NLS_DOUBLE, a.rows(), a.columns(), a.sparseDataPointer());
        return ArrayOf(NLS_DOUBLE, a.dimensions(), res, true);
    } break;
    case NLS_DCOMPLEX: {
        void* res = sparsedouble_uminus<doublecomplex>(
            NLS_DCOMPLEX, a.rows(), a.columns(), a.sparseDataPointer());
        return ArrayOf(NLS_DCOMPLEX, a.dimensions(), res, true);
    } break;
    case NLS_SINGLE: {
        void* res
            = sparsedouble_uminus<single>(NLS_SINGLE, a.rows(), a.columns(), a.sparseDataPointer());
        return ArrayOf(NLS_SINGLE, a.dimensions(), res, true);
    } break;
    case NLS_SCOMPLEX: {
        void* res = sparsedouble_uminus<singlecomplex>(
            NLS_SCOMPLEX, a.rows(), a.columns(), a.sparseDataPointer());
        return ArrayOf(NLS_SCOMPLEX, a.dimensions(), res, true);
    } break;
    default: {
        Error("Nelson:types:uminusUnsupportedType", "Cannot do uminus with current type '%s'.",
            ClassName(a));
    } break;
    }
    return {};
}
//=============================================================================
ArrayOf
sparselogical_uminus(const ArrayOf& a)
{
    Eigen::SparseMatrix<double, 0, signedIndexType>* res
        = (Eigen::SparseMatrix<double, 0, signedIndexType>*)Eigen_TypeConvertSparse(
            NLS_DOUBLE, a.rows(), a.columns(), a.sparseDataPointer(), NLS_LOGICAL);
    for (signedIndexType k = 0; k < res->outerSize(); ++k) {
        for (Eigen::SparseMatrix<double, 0, signedIndexType>::InnerIterator it(*res, k); it; ++it) {
            res->coeffRef(it.row(), it.col()) = -it.value();
        }
    }
    res->finalize();
    res->makeCompressed();
    return ArrayOf(NLS_DOUBLE, a.dimensions(), (void*)res, true);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
