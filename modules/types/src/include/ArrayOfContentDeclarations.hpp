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
 * Return scalar text as a UTF-8 string.
 */
[[nodiscard]] std::string
asUtf8String() const;
/**
 * Return scalar text as a wide string.
 */
[[nodiscard]] std::wstring
asWideString(size_t lengthMax = std::string::npos) const;
/**
 * Return character-array contents as wide text.
 */
[[nodiscard]] std::wstring
asCharacterArrayWideString() const;
/**
 * Return scalar text as a newly allocated UTF-8 C string.
 */
[[nodiscard]] char*
toAllocatedUtf8CString() const;
/**
 * Return scalar text as a newly allocated wide C string.
 */
[[nodiscard]] wchar_t*
toAllocatedWideCString() const;
/**
 * Return cell string contents as a vector without enforcing orientation.
 */
[[nodiscard]] wstringVector
asWideStringVector(bool bCheckVector = true) const;
[[nodiscard]] stringVector
asUtf8StringVector(bool bCheckVector = true) const;
/**
 * Return cell string contents and require a row vector shape.
 */
[[nodiscard]] wstringVector
asWideStringRowVector() const;
[[nodiscard]] stringVector
asUtf8StringRowVector() const;
/**
 * Return cell string contents and require a column vector shape.
 */
[[nodiscard]] wstringVector
asWideStringColumnVector() const;
[[nodiscard]] stringVector
asUtf8StringColumnVector() const;
/**
 * Return the value as a logical scalar.
 */
[[nodiscard]] logical
asLogicalScalar(bool arrayAsScalar = false) const;
/**
 * Return the value as a signed 8-bit scalar.
 */
[[nodiscard]] int8
asInt8Scalar(bool arrayAsScalar = false, bool checkIsIntegerValue = false) const;
/**
 * Return the value as an unsigned 8-bit scalar.
 */
[[nodiscard]] uint8
asUint8Scalar(bool arrayAsScalar = false, bool checkIsIntegerValue = false) const;
/**
 * Return the value as a signed 16-bit scalar.
 */
[[nodiscard]] int16
asInt16Scalar(bool arrayAsScalar = false, bool checkIsIntegerValue = false) const;
/**
 * Return the value as an unsigned 16-bit scalar.
 */
[[nodiscard]] uint16
asUint16Scalar(bool arrayAsScalar = false, bool checkIsIntegerValue = false) const;
/**
 * Return the value as a signed 32-bit scalar.
 */
[[nodiscard]] int32
asInt32Scalar(bool arrayAsScalar = false, bool checkIsIntegerValue = false) const;
/**
 * Return the value as an unsigned 32-bit scalar.
 */
[[nodiscard]] uint32
asUint32Scalar(bool arrayAsScalar = false, bool checkIsIntegerValue = false) const;
/**
 * Return the value as a double scalar.
 */
[[nodiscard]] double
asDoubleScalar(bool arrayAsScalar = false, bool checkIsIntegerValue = false) const;
/**
 * Return the value as a double vector.
 */
[[nodiscard]] std::vector<double>
asDoubleVector() const;
/**
 * Return the value as an unsigned 64-bit scalar.
 */
[[nodiscard]] uint64
asUint64Scalar(bool arrayAsScalar = false, bool checkIsIntegerValue = false) const;
/**
 * Return the value as a signed 64-bit scalar.
 */
[[nodiscard]] int64
asInt64Scalar(bool arrayAsScalar = false, bool checkIsIntegerValue = false) const;
/**
 * Return the value as an index scalar.
 */
[[nodiscard]] indexType
asScalarIndex(bool bWithZero = true, bool checkIsIntegerValue = false,
    bool convertNegativeValueAsZero = false) const;
/**
 * Return index storage as a raw pointer.
 */
indexType*
indexPointer();
/**
 * Return the value as a vector of indexes.
 */
[[nodiscard]] std::vector<indexType>
asIndexVector();
/**
 * Return the value as a double complex scalar.
 */
[[nodiscard]] doublecomplex
asDoubleComplexScalar(bool arrayAsScalar = false) const;
/**
 * Return the value as a single scalar.
 */
[[nodiscard]] single
asSingleScalar(bool arrayAsScalar = false, bool checkIsIntegerValue = false) const;
/**
 * Return the value as a single complex scalar.
 */
[[nodiscard]] std::complex<single>
asSingleComplexScalar(bool arrayAsScalar = false) const;
/**
 * Return the number of nonzero elements.
 *
 * Reference classes use the implementation's structural estimate.
 */
[[nodiscard]] indexType
nnz() const;
/**
 * Return sparse nonzero capacity.
 */
[[nodiscard]] indexType
nzmax() const;
/**
 * Preserve the requested sparse nonzero capacity.
 */
void
setSparseNzmax(indexType requestedNzmax);
/**
 * Return the number of logical elements.
 */
[[nodiscard]] indexType
numel() const;
/**
 * Return whether the class tag is cell.
 */
[[nodiscard]] bool
isCell() const;
/**
 * Return whether the value is a cell array of character vectors.
 */
[[nodiscard]] bool
isCellArrayOfCharacterVectors() const;
/**
 * Return whether the class tag is logical.
 */
[[nodiscard]] bool
isLogical() const;
/**
 * Return whether the value is a dense non-scalar logical array.
 */
[[nodiscard]] bool
isNdArrayLogical() const;
/**
 * Return whether the value is a sparse logical array.
 */
[[nodiscard]] bool
isSparseLogicalType() const;
/**
 * Return whether the class tag is numeric.
 */
[[nodiscard]] bool
isNumeric() const;
/**
 * Return the largest positive ordinal index stored in this value.
 *
 * The value is expected to have passed through toOrdinalType().
 */
indexType
maxAsIndex();
/**
 * Delete a payload block according to its runtime class.
 */
void
deleteArrayOf(void* dp, NelsonType dataclass);
//=============================================================================
