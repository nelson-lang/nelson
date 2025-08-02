//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include "ArrayOf.hpp"
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
    EyeSparseMatrixConstructorDynamicFunction(NelsonType dclass, indexType rows, indexType cols);
    //=============================================================================
    NLSTYPES_IMPEXP void*
    LogicalSparseMatrixConstructorDynamicFunction(indexType rows, indexType cols, bool bMotif);
    //=============================================================================
    NLSTYPES_IMPEXP void
    DeleteSparseMatrixDynamicFunction(NelsonType dclass, indexType rows, indexType cols, void* cp);
    //=============================================================================
    NLSTYPES_IMPEXP void*
    MakeDenseArrayOfDynamicFunction(
        NelsonType dclass, indexType rows, indexType cols, const void* cp);
    //=============================================================================
    NLSTYPES_IMPEXP void*
    MakeSparseArrayOfDynamicFunction(
        NelsonType dclass, indexType rows, indexType cols, const void* cp);
    //=============================================================================
    NLSTYPES_IMPEXP void*
    TypeConvertSparseDynamicFunction(
        NelsonType dclass, indexType rows, indexType cols, const void* cp, NelsonType oclass);
    //=============================================================================
    NLSTYPES_IMPEXP void*
    CopySparseMatrixDynamicFunction(
        NelsonType dclass, indexType rows, indexType cols, const void* cp);
    //=============================================================================
    NLSTYPES_IMPEXP indexType
    CountNonzerosDynamicFunction(NelsonType dclass, indexType rows, indexType cols, const void* cp);
    //=============================================================================
    NLSTYPES_IMPEXP indexType
    CountNonzerosMaxDynamicFunction(
        NelsonType dclass, indexType rows, indexType cols, const void* cp);
    //=============================================================================
    NLSTYPES_IMPEXP void*
    SparseMatrixConstructorDynamicFunction(
        NelsonType dclass, indexType rows, indexType cols, ArrayOfMatrix m);
    //=============================================================================
    NLSTYPES_IMPEXP void*
    GetSparseVectorSubsetsDynamicFunction(NelsonType dclass, indexType rows, indexType cols,
        const void* src, const indexType* indx, indexType irows, indexType icols);
    //=============================================================================
    NLSTYPES_IMPEXP void*
    GetSparseNDimSubsetsDynamicFunction(NelsonType dclass, indexType rows, indexType cols,
        const void* src, const indexType* rindx, indexType irows, const indexType* cindx,
        indexType icols);
    //=============================================================================
    NLSTYPES_IMPEXP void*
    SetSparseVectorSubsetsDynamicFunction(NelsonType dclass, indexType& rows, indexType& cols,
        const void* src, const indexType* indx, indexType irows, indexType icols, const void* data,
        int advance);
    //=============================================================================
    NLSTYPES_IMPEXP void*
    SetSparseNDimSubsetsDynamicFunction(NelsonType dclass, indexType& rows, indexType& cols,
        const void* src, const indexType* rindx, indexType irows, const indexType* cindx,
        indexType icols, const void* data, int advance);
    //=============================================================================
    NLSTYPES_IMPEXP void*
    GetSparseScalarElementDynamicFunction(NelsonType dclass, indexType rows, indexType cols,
        const void* src, indexType rindx, indexType cindx);
    //=============================================================================
    NLSTYPES_IMPEXP void*
    SparseToIJVDynamicFunction(NelsonType dclass, indexType rows, indexType cols, const void* cp,
        indexType*& I, indexType*& J, int& nnz);
    //=============================================================================
    NLSTYPES_IMPEXP void*
    DeleteSparseMatrixColsDynamicFunction(
        NelsonType dclass, indexType rows, indexType cols, const void* cp, bool* dmap);
    //=============================================================================
    NLSTYPES_IMPEXP void*
    DeleteSparseMatrixRowsDynamicFunction(
        NelsonType dclass, indexType rows, indexType cols, const void* cp, bool* dmap);
    //=============================================================================
    NLSTYPES_IMPEXP void*
    DeleteSparseMatrixVectorSubsetDynamicFunction(NelsonType dclass, indexType& rows,
        indexType& cols, const void* cp, const indexType* todel, indexType delete_len);
    //=============================================================================
    NLSTYPES_IMPEXP void*
    ReshapeSparseMatrixDynamicFunction(NelsonType dclass, indexType rows, indexType cols,
        indexType newrows, indexType newcols, const void* cp);
    //=============================================================================

    //=============================================================================
#ifdef __cplusplus
}
#endif
//=============================================================================
