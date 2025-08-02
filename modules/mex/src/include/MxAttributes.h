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
#include "nlsMex_exports.h"
//=============================================================================
#ifdef __cplusplus
extern "C"
{
#endif
    //=============================================================================
    /**
     * Determine whether array is scalar array
     * @param Pointer to an mxArray
     * @return Logical 1 (true) or logical 0 (false).
     */
    NLSMEX_IMPEXP
    bool
    mxIsScalar(const mxArray* array_ptr);
    //=============================================================================
    /**
     * Determine whether array is empty.
     * @param Pointer to an mxArray
     * @return Logical 1 (true) or logical 0 (false).
     */
    NLSMEX_IMPEXP
    bool
    mxIsEmpty(const mxArray* pm);
    //=============================================================================
    /**
     * Offset from first element to desired element.
     * @param Pointer to an mxArray
     * @param Number of elements in the subs array
     * @param An array of integer that specifies dimension's subscript
     * @return Number of elements between the begin of the mxArray and the specified subscript.
     */
    NLSMEX_IMPEXP
    mwIndex
    mxCalcSingleSubscript(const mxArray* pm, mwSize nsubs, mwIndex* subs);
    //=============================================================================
    /**
     * Modify number of dimensions and size of each dimension.
     * @param Pointer to an mxArray
     * @param Dimensions array contains the size of the array in that dimension
     * @param Number of dimensions
     * @return 0 on success, and 1 on failure.
     */
    NLSMEX_IMPEXP
    int
    mxSetDimensions(mxArray* pm, const mwSize* dims, mwSize ndim);
    //=============================================================================
    /**
     * Pointer to dimensions array.
     * @param Pointer to an mxArray
     * @return Pointer on the first element in the dimensions array.
     */
    NLSMEX_IMPEXP
    const mwSize*
    mxGetDimensions(const mxArray* pm);
    //=============================================================================
    /**
     * Number of bytes required to store each data element.
     * @param Pointer to an mxArray
     * @return Number of bytes.
     */
    NLSMEX_IMPEXP
    size_t
    mxGetElementSize(const mxArray* pm);
    //=============================================================================
    /**
     * Number of dimensions in array.
     * @param Pointer to an mxArray
     * @return Number of dimension.
     */
    NLSMEX_IMPEXP
    mwSize
    mxGetNumberOfDimensions(const mxArray* pm);
    //=============================================================================
    /**
     * Number of elements in numeric array.
     * @param Pointer to an mxArray
     * @return Number of elements
     */
    NLSMEX_IMPEXP
    size_t
    mxGetNumberOfElements(const mxArray* pm);
    //=============================================================================
    /**
     * Number of rows in array.
     * @param Pointer to an mxArray
     * @return Number of rows.
     */
    NLSMEX_IMPEXP
    size_t
    mxGetM(const mxArray* pm);
    //=============================================================================
    /**
     * Number of columns in array.
     * @param Pointer to an mxArray
     * @return Number of columns.
     */
    NLSMEX_IMPEXP
    size_t
    mxGetN(const mxArray* pm);
    //=============================================================================
    /**
     * Set number of rows in array.
     * @param Pointer to an mxArray
     * @param Number of rows
     */
    NLSMEX_IMPEXP
    void
    mxSetM(mxArray* pm, mwSize m);
    //=============================================================================
    /**
     * Set number of columns in array.
     * @param Pointer to an mxArray
     * @param Number of columns
     */
    NLSMEX_IMPEXP
    void
    mxSetN(mxArray* pm, mwSize n);
    //=============================================================================
    /**
     * Determine whether array is numeric.
     * @param Pointer to an mxArray
     * @return 1 if it is an numerical array.
     */
    NLSMEX_IMPEXP
    bool
    mxIsNumeric(const mxArray* pm);
    //=============================================================================
    /**
     * Determine whether data is complex.
     * @param Pointer to an mxArray
     * @return 1 if it is an complex array.
     */
    NLSMEX_IMPEXP
    bool
    mxIsComplex(const mxArray* pm);
    //=============================================================================
#ifdef __cplusplus
}
#endif
//=============================================================================
