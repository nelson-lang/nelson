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
    switch (a.getDataClass()) {
    case NLS_DOUBLE: {
        void* res = sparsedouble_uminus<double>(
            NLS_DOUBLE, a.getRows(), a.getColumns(), a.getSparseDataPointer());
        return ArrayOf(NLS_DOUBLE, a.getDimensions(), res, true);
    } break;
    case NLS_DCOMPLEX: {
        void* res = sparsedouble_uminus<doublecomplex>(
            NLS_DCOMPLEX, a.getRows(), a.getColumns(), a.getSparseDataPointer());
        return ArrayOf(NLS_DCOMPLEX, a.getDimensions(), res, true);
    } break;
    default: {
        Error(_("Cannot do uminus with current type") + " '" + ClassName(a) + "'.");
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
            NLS_DOUBLE, a.getRows(), a.getColumns(), a.getSparseDataPointer(), NLS_LOGICAL);
    for (signedIndexType k = 0; k < res->outerSize(); ++k) {
        for (Eigen::SparseMatrix<double, 0, signedIndexType>::InnerIterator it(*res, k); it; ++it) {
            res->coeffRef(it.row(), it.col()) = -it.value();
        }
    }
    res->finalize();
    res->makeCompressed();
    return ArrayOf(NLS_DOUBLE, a.getDimensions(), (void*)res, true);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
