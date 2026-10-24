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
 * Return the optional value name attached to this array.
 */
[[nodiscard]] std::string
name() const;
/**
 * Set the optional value name attached to this array.
 */
void
name(const std::string& name);
/**
 * Return the optional value name as wide text.
 */
[[nodiscard]] std::wstring
wname() const;
/**
 * Return a deep copy of this value.
 */
ArrayOf
clone() const;
/**
 * Allocate payload storage for a Nelson class.
 */
static void*
allocateArrayOf(NelsonType /*type*/, indexType length, const stringVector& names = stringVector(),
    bool initializeValues = false);
/**
 * Convert this value to ordinal index storage in place.
 *
 * Logical arrays become the indexes of nonzero elements. Numeric arrays are
 * converted by truncating to index values, with the implementation reporting
 * invalid, fractional, or non-real inputs through its normal diagnostics.
 * Reference and text classes are not valid ordinal indexes.
 */
void
toOrdinalType();
/**
 * Detach shared storage before a write.
 */
void
ensureSingleOwner();
/**
 * Default constructor.
 */
ArrayOf();
/**
 * Copy constructor.
 */
ArrayOf(const ArrayOf& copy);
/**
 * Move constructor.
 */
ArrayOf(ArrayOf&& move) noexcept;
/**
 * Create an empty ArrayOf of the specified type.
 */
ArrayOf(NelsonType type);
/**
 * Create an ArrayOf with the specified contents.
 */
ArrayOf(NelsonType, const Dimensions&, void*, bool sparse = false,
    const stringVector& = stringVector());
/**
 * Create a scalar ArrayOf with inline data (no heap allocation for payload).
 */
ArrayOf(NelsonType type, const Dimensions& dims, const void* scalarData, size_t dataSize);
/**
 * Release the storage reference owned by this value.
 */
~ArrayOf();
/**
 * Assignment operator.
 */
void
operator=(const ArrayOf& copy);
/**
 * Move assignment operator.
 */
void
operator=(ArrayOf&& move) noexcept;
/**
 * Return the Data owner count for diagnostics.
 */
[[nodiscard]] indexType
referenceCount() const;
/**
 * Return the first dimension length.
 */
[[nodiscard]] indexType
rows() const;
/**
 * Return the second dimension length.
 */
[[nodiscard]] indexType
columns() const;
/**
 * Return the number of active dimensions.
 */
[[nodiscard]] indexType
nDims() const;
/**
 * Return the number of logical elements.
 */
[[nodiscard]] indexType
elementCount() const;
/**
 * Return a copy of the array dimensions.
 */
[[nodiscard]] Dimensions
dimensions() const;
/**
 * Return field names for structure-like arrays.
 */
[[nodiscard]] stringVector
fieldNames() const;
/**
 * Return the length of one dimension.
 */
[[nodiscard]] indexType
dimensionLength(int) const;
/**
 * Return a read-only pointer to dense payload storage.
 *
 * The pointer is valid while this ArrayOf keeps the same storage. Use
 * readWriteDataPointer() before mutating payload data.
 */
[[nodiscard]] const void*
dataPointer() const;
/**
 * Return a read-only pointer to sparse payload storage.
 */
[[nodiscard]] const void*
sparseDataPointer() const;
/**
 * Return writable sparse payload storage after detaching shared storage.
 */
void*
sparseReadWriteDataPointer();
/**
 * Return writable dense payload storage.
 *
 * Shared storage is detached before the pointer is returned.
 */
void*
readWriteDataPointer();
/**
 * Replace the payload pointer and transfer ownership to this ArrayOf.
 *
 * Callers must not free or write through the pointer after the transfer.
 */
void
setDataPointer(void*);
/**
 * Resize to new dimensions while keeping existing values at the origin.
 */
void
resize(Dimensions& a);
/**
 * Resize so a one-dimensional indexing assignment can address index.
 *
 * Scalars and row vectors grow along columns; column vectors grow along
 * rows; other shapes become a row vector of the requested length.
 */
void vectorResize(indexType);
/**
 * Change dimensions without changing element count.
 */
void
reshape(Dimensions& a, bool checkValidDimension = true);
/**
 * Change dimensions in place for storage classes that support it.
 */
void
changeInPlaceDimensions(const Dimensions& a);
/**
 * Return the runtime class tag.
 */
[[nodiscard]] NelsonType
dataClass() const;
/**
 * Return the size in bytes of one element.
 */
[[nodiscard]] indexType
elementSize() const;
/**
 * Return the byte count required by the exposed elements.
 */
[[nodiscard]] indexType
byteSize() const;
/**
 * Return whether every stored value is positive.
 *
 * Unsigned integer arrays satisfy this by class. Signed integers and real
 * floating-point arrays are scanned. Complex arrays do not satisfy it.
 */
[[nodiscard]] bool
isPositive() const;
/**
 * Return whether this value uses sparse storage.
 */
[[nodiscard]] bool
isSparse() const;
/**
 * Convert dense storage to sparse storage when the class supports it.
 */
void
makeSparse();
/**
 * Convert sparse storage to dense storage when the class supports it.
 */
void
makeDense();
/**
 * Return the number of explicitly stored nonzero elements.
 */
[[nodiscard]] indexType
nonzeros() const;
/**
 * Return whether this scalar matches x under case-comparison rules.
 */
[[nodiscard]] bool
testCaseMatchScalar(ArrayOf x) const;
/**
 * Return whether this value matches a scalar case label or one cell entry.
 */
[[nodiscard]] bool
testForCaseMatch(ArrayOf x) const;
/**
 * Return whether the array has no elements.
 */
[[nodiscard]] bool
isEmpty(bool allDimensionsIsZero = false) const;
/**
 * Return whether the array has exactly one element.
 */
[[nodiscard]] bool
isScalar() const;
/**
 * Return whether the array has exactly two active dimensions.
 */
[[nodiscard]] bool
is2D() const;
/**
 * Return whether the array is a two-dimensional square matrix.
 */
[[nodiscard]] bool
isSquare() const;
/**
 * Return whether the array is a row or column vector.
 */
[[nodiscard]] bool
isVector() const;
/**
 * Return whether the array is a row vector.
 */
[[nodiscard]] bool
isRowVector() const;
/**
 * Return whether the array is a column vector.
 */
[[nodiscard]] bool
isColumnVector() const;
/**
 * Return whether the array stores nested ArrayOf values or handles.
 */
[[nodiscard]] bool
isReferenceType() const;
/**
 * Return whether the storage class is complex.
 */
[[nodiscard]] bool
isComplex() const;
/**
 * Return whether the storage class is real-valued.
 */
[[nodiscard]] bool
isReal() const;
/**
 * Return whether all stored values have zero imaginary part.
 */
[[nodiscard]] bool
allReal() const;
/**
 * Return whether the array is sparse double storage.
 */
[[nodiscard]] bool
isSparseDoubleType(bool realOnly = false) const;
/**
 * Return whether the array is dense non-scalar double matrix storage.
 */
[[nodiscard]] bool
isNdArrayDoubleType(bool realOnly = false) const;
/**
 * Return whether the array is dense scalar double storage.
 */
[[nodiscard]] bool
isDoubleType(bool realOnly = false) const;
/**
 * Return whether the array is dense scalar single storage.
 */
[[nodiscard]] bool
isSingleType(bool realOnly = false) const;
/**
 * Return whether the class tag is double precision, real or complex.
 */
[[nodiscard]] bool
isDoubleClass() const;
/**
 * Return whether the class tag is single precision, real or complex.
 */
[[nodiscard]] bool
isSingleClass() const;
/**
 * Return whether the array is dense non-scalar single matrix storage.
 */
[[nodiscard]] bool
isNdArraySingleType(bool realOnly = false) const;
/**
 * Return whether the array stores character data.
 */
[[nodiscard]] bool
isCharacterArray() const;
/**
 * Return whether the array is a row vector of characters.
 */
[[nodiscard]] bool
isRowVectorCharacterArray() const;
/**
 * Return whether the array is a non-scalar character array.
 */
[[nodiscard]] bool
isNdArrayCharacterType() const;
/**
 * Return whether the class tag is an integer class.
 */
[[nodiscard]] bool
isIntegerType() const;
/**
 * Return whether the stored value can be used as an integer value.
 */
[[nodiscard]] bool
isIntegerValue() const;
/**
 * Return whether the array is a dense non-scalar integer array.
 */
[[nodiscard]] bool
isNdArrayIntegerType() const;
/**
 * Return whether the class tag is an unsigned integer class.
 */
[[nodiscard]] bool
isUnsignedIntegerType() const;
/**
 * Return whether the class tag is a signed integer class.
 */
[[nodiscard]] bool
isSignedIntegerType() const;
/**
 * Deep-copy elements into an existing destination buffer.
 *
 * srcIndex, dstIndex, and count are element indexes, not byte offsets.
 */
void
copyElements(indexType srcIndex, void* dstPtr, indexType dstIndex, indexType count);
/**
 * Promote this value to a new runtime class.
 *
 * Numeric classes are converted through the normal promotion rules. Empty
 * arrays may be retagged. Structure-like arrays may also reorder or extend
 * fields when the requested field list is compatible.
 */
void
promoteType(NelsonType new_type, const stringVector& fieldNames);
/**
 * Promote this value when no destination field list is required.
 */
void
promoteType(NelsonType new_type);
/**
 * Return whether promotion to new_type is supported.
 */
bool
canBePromotedTo(NelsonType new_type);
//=============================================================================
