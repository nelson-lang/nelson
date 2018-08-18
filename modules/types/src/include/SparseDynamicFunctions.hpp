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
#pragma once
//=============================================================================
#include "ArrayOf.hpp"
#include "Error.hpp"
#include "Types.hpp"
#include "nlsTypes_exports.h"
//=============================================================================
using namespace Nelson;
//=============================================================================
#ifdef __cplusplus
extern "C"
{
#endif
    //=============================================================================
    NLSTYPES_IMPEXP void*
    EyeSparseMatrixConstructorDynamicFunction(Class dclass, indexType rows, indexType cols);
    //=============================================================================
    NLSTYPES_IMPEXP void*
    LogicalSparseMatrixConstructorDynamicFunction(indexType rows, indexType cols, bool bMotif);
    //=============================================================================
    NLSTYPES_IMPEXP void
    DeleteSparseMatrixDynamicFunction(Class dclass, indexType rows, indexType cols, void* cp);
    //=============================================================================
    NLSTYPES_IMPEXP void*
    MakeDenseArrayOfDynamicFunction(Class dclass, indexType rows, indexType cols, const void* cp);
    //=============================================================================
    NLSTYPES_IMPEXP void*
    MakeSparseArrayOfDynamicFunction(Class dclass, indexType rows, indexType cols, const void* cp);
    //=============================================================================
    NLSTYPES_IMPEXP void*
    TypeConvertSparseDynamicFunction(
        Class dclass, indexType rows, indexType cols, const void* cp, Class oclass);
    //=============================================================================
    NLSTYPES_IMPEXP void*
    CopySparseMatrixDynamicFunction(Class dclass, indexType rows, indexType cols, const void* cp);
    //=============================================================================
    NLSTYPES_IMPEXP indexType
    CountNonzerosDynamicFunction(Class dclass, indexType rows, indexType cols, const void* cp);
    //=============================================================================
    NLSTYPES_IMPEXP indexType
    CountNonzerosMaxDynamicFunction(Class dclass, indexType rows, indexType cols, const void* cp);
    //=============================================================================
    NLSTYPES_IMPEXP void*
    SparseMatrixConstructorDynamicFunction(
        Class dclass, indexType rows, indexType cols, ArrayOfMatrix m);
    //=============================================================================
    NLSTYPES_IMPEXP void*
    GetSparseVectorSubsetsDynamicFunction(Class dclass, indexType rows, indexType cols,
        const void* src, const indexType* indx, indexType irows, indexType icols);
    //=============================================================================
    NLSTYPES_IMPEXP void*
    GetSparseNDimSubsetsDynamicFunction(Class dclass, indexType rows, indexType cols,
        const void* src, const indexType* rindx, indexType irows, const indexType* cindx,
        indexType icols);
    //=============================================================================
    NLSTYPES_IMPEXP void*
    SetSparseVectorSubsetsDynamicFunction(Class dclass, indexType& rows, indexType& cols,
        const void* src, const indexType* indx, indexType irows, indexType icols, const void* data,
        int advance);
    //=============================================================================
    NLSTYPES_IMPEXP void*
    SetSparseNDimSubsetsDynamicFunction(Class dclass, indexType& rows, indexType& cols,
        const void* src, const indexType* rindx, indexType irows, const indexType* cindx,
        indexType icols, const void* data, int advance);
    //=============================================================================
    NLSTYPES_IMPEXP void*
    GetSparseScalarElementDynamicFunction(Class dclass, indexType rows, indexType cols,
        const void* src, indexType rindx, indexType cindx);
    //=============================================================================
    NLSTYPES_IMPEXP void*
    SparseToIJVDynamicFunction(Class dclass, indexType rows, indexType cols, const void* cp,
        indexType*& I, indexType*& J, int& nnz);
    //=============================================================================
    NLSTYPES_IMPEXP void*
    DeleteSparseMatrixColsDynamicFunction(
        Class dclass, indexType rows, indexType cols, const void* cp, bool* dmap);
    //=============================================================================
    NLSTYPES_IMPEXP void*
    DeleteSparseMatrixRowsDynamicFunction(
        Class dclass, indexType rows, indexType cols, const void* cp, bool* dmap);
    //=============================================================================
    NLSTYPES_IMPEXP void*
    DeleteSparseMatrixVectorSubsetDynamicFunction(Class dclass, indexType& rows, indexType& cols,
        const void* cp, const indexType* todel, indexType delete_len);
    //=============================================================================
    NLSTYPES_IMPEXP void*
    ReshapeSparseMatrixDynamicFunction(Class dclass, indexType rows, indexType cols,
        indexType newrows, indexType newcols, const void* cp);
    //=============================================================================

#ifdef __cplusplus
}
#endif
//=============================================================================
