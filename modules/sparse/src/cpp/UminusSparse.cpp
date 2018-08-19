//=============================================================================
// Copyright (c) 2016-2018 Allan CORNET (Nelson)
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#define _SCL_SECURE_NO_WARNINGS
//=============================================================================
#include "UminusSparse.hpp"
#include "ClassName.hpp"
#include "Error.hpp"
#include "SparseType.hpp"
#include <Eigen/Dense>
#include <Eigen/Sparse>
//=============================================================================
namespace Nelson {
//=============================================================================
template <class T>
void*
sparsedouble_uminus(Class dclass, indexType rows, indexType cols, const void* cp)
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
sparsedouble_uminus(ArrayOf a)
{
    switch (a.getDataClass()) {
    case NLS_DOUBLE: {
        void* res = sparsedouble_uminus<double>(NLS_DOUBLE, a.getDimensions().getRows(),
            a.getDimensions().getColumns(), a.getSparseDataPointer());
        return ArrayOf(NLS_DOUBLE, a.getDimensions(), (void*)res, true);
    } break;
    case NLS_DCOMPLEX: {
        void* res = sparsedouble_uminus<doublecomplex>(NLS_DCOMPLEX, a.getDimensions().getRows(),
            a.getDimensions().getColumns(), a.getSparseDataPointer());
        return ArrayOf(NLS_DCOMPLEX, a.getDimensions(), (void*)res, true);
    } break;
    default: {
        Error(_("Cannot do uminus with current type") + " '" + ClassName(a) + "'.");
    } break;
    }
    return ArrayOf();
}
//=============================================================================
ArrayOf
sparselogical_uminus(ArrayOf a)
{
    Eigen::SparseMatrix<double, 0, signedIndexType>* res
        = (Eigen::SparseMatrix<double, 0, signedIndexType>*)Eigen_TypeConvertSparse(NLS_DOUBLE,
            a.getDimensions().getRows(), a.getDimensions().getColumns(), a.getSparseDataPointer(),
            NLS_LOGICAL);
    for (signedIndexType k = 0; k < res->outerSize(); ++k) {
        for (Eigen::SparseMatrix<double, 0, signedIndexType>::InnerIterator it(*res, k); it; ++it) {
            res->coeffRef(it.row(), it.col()) = -it.value();
        }
    }
    res->finalize();
    res->makeCompressed();
    return ArrayOf(NLS_DOUBLE, a.getDimensions(), (void*)res, true);
}
}
//=============================================================================
