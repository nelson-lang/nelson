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
 * Construct a known scalar directly, without constructing a shape descriptor.
 */
ArrayOf(Representation representation, double value) noexcept
    : representation_(representation), scalarStorage_ { value }
{
}
//=============================================================================
ArrayOf(Representation representation, logical value) noexcept
    : representation_(representation), scalarType_(NLS_LOGICAL)
{
    scalarStorage_.logicalValue = value;
}
//=============================================================================
ArrayOf(Representation representation, single value) noexcept
    : representation_(representation), scalarType_(NLS_SINGLE)
{
    scalarStorage_.singleValue = value;
}
//=============================================================================
/**
 * Build a deletion or selection map from ordinal indexes.
 *
 * The returned array has one slot per accepted ordinal value. Each input
 * index is expected to be in the range [1, maxD].
 */
bool* getBinaryMap(indexType);
Data*
putOrdinalData(const Dimensions& dimensions, indexType* values);
/**
 * Build ordinal index storage from a logical array.
 */
Data*
logicalOrdinalData(indexType elementCount, bool isRowVector);
/**
 * Gather elements selected by a dense logical mask in one typed pass.
 *
 * Handles plain POD element classes only; returns false when the caller
 * must fall back to the generic ordinal-index extraction.
 */
bool
tryLogicalMaskGather(const ArrayOf& mask, ArrayOf& result);
/**
 * Gather elements selected by a dense real numeric ordinal index in one
 * pass (convert, bound check and copy, no intermediate index array).
 *
 * Returns false when the caller must fall back to the generic path: an
 * unsupported class, or any invalid index (so that path raises the error).
 */
bool
tryOrdinalGather(const ArrayOf& index, ArrayOf& result);
/**
 * Output dimensions of a linear subscript: the index shape, except that a
 * vector index of a (non-scalar) vector keeps the source orientation.
 */
Dimensions
linearSubsetDimensions(const ArrayOf& index) const;
/**
 * Write A(I1,I2,...) = B as one contiguous block per column when the first
 * subscript is ':' or an ascending unit-stride run.
 *
 * Returns false (nothing written) when the shape does not qualify; the
 * caller then runs the element-wise assignment loop.
 */
static bool
trySetContiguousBlocks(ArrayOf& rightData, void* destination, const indexType outDims[maxDims],
    const indexType srcDims[maxDims], constIndexPtr* indexes, indexType indexCount,
    indexType colonIndex, indexType advance);
/**
 * Build ordinal index storage from real floating-point data.
 */
template <class T>
Data*
realOrdinalData(indexType elementCount, indexType stride);
/**
 * Build ordinal index storage from signed or unsigned integer data.
 */
template <class T>
Data*
integerOrdinalData(indexType elementCount);
/**
 * Build ordinal index storage from uint64 data.
 */
Data*
uint64OrdinalData(indexType elementCount);
/**
 * Copy another ArrayOf into this object, including storage ownership.
 */
void
copyObject(const ArrayOf& copy);
/**
 * Transfer storage, compacting an already materialized scalar when possible.
 */
void
moveObject(ArrayOf&& move) noexcept;
/**
 * Release the current storage reference.
 */
void
deleteContents();
/**
 * Return whether this value stores a scalar without a Data block.
 */
[[nodiscard]] bool
hasScalarStorage() const noexcept;
/**
 * Return whether an existing Data block can become compact scalar storage.
 */
bool
canUseScalarData() const;
/**
 * Size of the supported compact scalar types, or zero for other types.
 */
static size_t
scalarStorageSize(NelsonType type) noexcept;
/**
 * Return the address of the active scalar value or owned buffer.
 */
const void*
scalarDataPointer() const noexcept;
/**
 * Store a scalar value without allocating a Data block.
 */
void
setInlineScalar(NelsonType type, const void* value, indexType dimensionCount = 2) noexcept;
/**
 * Install an owned scalar buffer after the previous contents have been released.
 */
void
setOwnedScalar(NelsonType type, void* data, indexType dimensionCount) noexcept;
/**
 * Move compact scalar storage into normal Data storage.
 */
void
materializeScalar();
/**
 * Adopt a dense scalar buffer without reading it or allocating a Data block.
 */
bool
tryAdoptScalarStorage(
    NelsonType type, const Dimensions& dims, void* data, bool sparse, const stringVector& fields);
/**
 * Replace storage, using compact scalar storage whenever possible.
 */
void
replaceData(NelsonType type, const Dimensions& dims, void* data, bool sparse = false,
    const stringVector& fields = stringVector());
/**
 * Return whether every field name is syntactically valid.
 */
static bool
haveValidFieldNames(const stringVector& fieldnames);
/**
 * Return whether field names contain no duplicates.
 */
static bool
haveUniqueFieldNames(const stringVector& fieldnames);
//=============================================================================
