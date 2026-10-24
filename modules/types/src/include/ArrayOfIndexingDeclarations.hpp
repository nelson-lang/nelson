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
// This declaration fragment is included inside class ArrayOf.
// Do not include it directly from other headers.
//=============================================================================
/**
 * Return one value by linear zero-based index.
 */
ArrayOf
valueAtIndex(uint64 index);
/**
 * Replace one element selected by linear zero-based index.
 */
void
setValueAtIndex(uint64 index, const ArrayOf& scalarValue);
/**
 * Replace this value with another ArrayOf value.
 */
void
setValue(const ArrayOf& value);
/**
 * Return a subset addressed by one linear index array.
 *
 * Indexes address elements in column-major order, as if the array had been
 * reshaped to a vector.
 */
ArrayOf
vectorSubset(ArrayOf& index);
/**
 * Return a subset addressed by one index array per dimension.
 */
ArrayOf
nDimSubset(ArrayOfVector& index);
/**
 * Return one cell content value using linear indexing.
 *
 * This path is used by assignments such as a{10} = value. The index must
 * select exactly one cell.
 */
ArrayOf
vectorContents(ArrayOf& index);
/**
 * Return one cell content value using multi-dimensional indexing.
 */
ArrayOf
nDimContents(ArrayOfVector& index);
/**
 * Return a list of cell contents selected by linear indexes.
 */
ArrayOfVector
vectorContentsAsList(ArrayOf& index);
/**
 * Return a list of cell contents selected by multi-dimensional indexes.
 */
ArrayOfVector
nDimContentsAsList(ArrayOfVector& index);
/**
 * Assign data into a subset selected by linear indexes.
 */
void
setVectorSubset(ArrayOf& index, ArrayOf& data);
/**
 * Assign data into a subset selected by multi-dimensional indexes.
 */
void
setNDimSubset(ArrayOfVector& index, ArrayOf& data);
/**
 * Assign one cell content value selected by a linear index.
 */
void
setVectorContents(ArrayOf& index, ArrayOf& data);
/**
 * Assign one cell content value selected by multi-dimensional indexes.
 */
void
setNDimContents(ArrayOfVector& index, ArrayOf& data);
/**
 * Replace one field in a scalar structure-like value.
 */
void
setField(const std::string& fieldName, ArrayOf& data);
/**
 * Assign a list of values into cell contents selected by linear indexes.
 *
 * This path supports multi-output assignments such as [x{3:5}] = foo.
 */
void
setVectorContentsAsList(ArrayOf& index, ArrayOfVector& data);
/**
 * Assign a list of values into cell contents selected by n-D indexes.
 *
 * This path supports multi-output assignments such as [x{1,3:5}] = foo.
 */
void
setNDimContentsAsList(ArrayOfVector& index, ArrayOfVector& data);
/**
 * Assign a list of values into one field across a structure array.
 */
void
setFieldAsList(const std::string& fieldName, ArrayOfVector& data);
/**
 * Delete elements selected by one linear index array.
 *
 * The result is reshaped using the same rules as vectorResize().
 */
void
deleteVectorSubset(ArrayOf& arg);
/**
 * Delete a full plane selected by n-dimensional indexes.
 *
 * This operation removes entries along one non-colon dimension and does not
 * create holes.
 */
void
deleteNDimSubset(ArrayOfVector& args);
//=============================================================================
