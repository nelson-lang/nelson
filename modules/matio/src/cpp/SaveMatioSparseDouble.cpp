//=============================================================================
// Copyright (c) 2016-2019 Allan CORNET (Nelson)
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
#include <Eigen/Sparse>
#include <algorithm>
#include <set>
#include "SaveMatioSparseDouble.hpp"
#include "matioHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
matvar_t*
SaveMatioSparseDouble(const std::string &variableName, ArrayOf variableValue)
{
    Dimensions variableDims = variableValue.getDimensions();
    indexType rank;
    size_t* dims = convertDimensionsForMatVar(variableDims, rank);
    if (dims == nullptr) {
        return nullptr;
    }
    Eigen::SparseMatrix<double, 0, signedIndexType>* spmat
        = (Eigen::SparseMatrix<double, 0, signedIndexType>*)variableValue.getSparseDataPointer();
    indexType nnz = 0;
    if (spmat) {
        nnz = spmat->nonZeros();
    }

    auto nzmax = static_cast<int32>(variableValue.nzmax());
    int njc = 0;
    if (spmat) {
        njc = (int)spmat->outerSize();
    }
    int nir = static_cast<int>(nnz);

    int32* pI = nullptr;
    try {
        pI = new int32[nir];
    } catch (const std::bad_alloc&) {
        return nullptr;
    }
    int32* pJ = nullptr;
    try {
        pJ = new int32[njc + 1];
    } catch (const std::bad_alloc&) {
        delete[] pI;
        return nullptr;
    }
    signedIndexType* pInner = nullptr;
    if (spmat) {
        pInner = spmat->innerIndexPtr();
        for (signedIndexType k = 0; k < nir; ++k) {
            pI[k] = static_cast<int32>(pInner[k]);
        }
    }
    signedIndexType* pOuter = nullptr;
    if (spmat) {
        pOuter = spmat->outerIndexPtr();
        for (signedIndexType k = 0; k < njc; ++k) {
            pJ[k] = static_cast<int32>(pOuter[k]);
        }
    }
    pJ[njc] = static_cast<int32>(nnz);
    mat_sparse_t* sparse = nullptr;
    try {
        sparse = new mat_sparse_t[1];
    } catch (const std::bad_alloc&) {
        delete[] pI;
        delete[] pJ;
        return nullptr;
    }

    sparse->nzmax = nzmax;
    sparse->nir = nir;
    sparse->ir = pI;
    sparse->njc = njc + 1;
    sparse->jc = pJ;
    sparse->ndata = (int)nnz;
    sparse->data = spmat->valuePtr();

    matvar_t* matVariableNoCopy = Mat_VarCreate(variableName.c_str(), MAT_C_SPARSE, MAT_T_DOUBLE,
        (int)rank, dims, sparse, MAT_F_DONT_COPY_DATA);
    if (matVariableNoCopy == nullptr) {
        delete[] dims;
        delete[] pI;
        delete[] pJ;
        delete[] sparse;
        return matVariableNoCopy;
    }
    matvar_t* matVariable = Mat_VarCreate(variableName.c_str(), MAT_C_SPARSE, MAT_T_DOUBLE,
        (int)rank, dims, matVariableNoCopy->data, 0);
    Mat_VarFree(matVariableNoCopy);
    delete[] dims;
    delete[] pI;
    delete[] pJ;
    delete[] sparse;
    return matVariable;
}
//=============================================================================
}  // namespace Nelson
//=============================================================================
