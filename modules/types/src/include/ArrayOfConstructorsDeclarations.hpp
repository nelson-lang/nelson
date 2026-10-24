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
 * Construct empty cell or value arrays.
 */
static ArrayOf
emptyCell(const Dimensions& dim);
static ArrayOf
empty(const Dimensions& dim, bool bIsSparse = false);
static ArrayOf
empty(indexType m = 0, indexType n = 0, bool bIsSparse = false);
/**
 * Build a diagonal matrix from a vector.
 */
static ArrayOf
diagonalMatrix(ArrayOf src, int64 diagonalOrder);
/**
 * Return one diagonal from a two-dimensional array.
 */
ArrayOf
diagonal(int64 diagonalOrder);
/**
 * Construct scalar logical and numeric values.
 */
static ArrayOf
logicalScalar(bool aval);
static ArrayOf
uint8Scalar(uint8 aval);
static ArrayOf
int8Scalar(int8 aval);
static ArrayOf
uint16Scalar(uint16 aval);
static ArrayOf
int16Scalar(int16 aval);
static ArrayOf
uint32Scalar(uint32 aval);
static ArrayOf
int32Scalar(int32 aval);
/**
 * Construct 64-bit integer scalar values.
 */
static ArrayOf
uint64Scalar(uint64 aval);
static ArrayOf
int64Scalar(int64 aval);
/**
 * Construct floating-point scalar values.
 */
static ArrayOf
singleScalar(float aval);
static ArrayOf
doubleScalar(double aval);
/**
 * Construct scalar complex values from real and imaginary parts.
 */
static ArrayOf
singleComplexScalar(float aval, float bval);
static ArrayOf
dsingleComplexScalar(double aval, double bval);
/**
 * Convert a string array to a character array.
 *
 * Missing string values become empty text only when missingAsEmpty is true.
 */
static ArrayOf
characterArrayFromStringArray(const ArrayOf& stringArray, bool missingAsEmpty = true);
/**
 * Build a character matrix from text rows.
 */
static ArrayOf
characterArrayFromRows(const wstringVector& strs, bool leftAlign = true);
/**
 * Build a character matrix from UTF-8 text rows.
 */
static ArrayOf
characterArrayFromRows(const stringVector& strs, bool leftAlign = true);
/**
 * Build a character row vector from UTF-8 text.
 */
static ArrayOf
characterArray(const std::string& astr);
/**
 * Build a character row vector from wide text.
 */
static ArrayOf
characterArray(const std::wstring& astr);
/**
 * Construct numeric row vectors with the requested length.
 */
static ArrayOf
uint8RowVector(indexType len);
static ArrayOf
int8RowVector(indexType len);
static ArrayOf
uint16RowVector(indexType len);
static ArrayOf
int16RowVector(indexType len);
static ArrayOf
uint32RowVector(indexType len);
static ArrayOf
int32RowVector(indexType len);
static ArrayOf
uint64RowVector(indexType len);
static ArrayOf
int64RowVector(indexType len);
/**
 * Construct numeric row vectors from explicit values.
 */
static ArrayOf
uint8RowVector(const std::vector<uint8>& values);
static ArrayOf
uint16RowVector(const std::vector<uint16>& values);
static ArrayOf
uint32RowVector(const std::vector<uint32>& values);
static ArrayOf
uint64RowVector(const std::vector<uint64>& values);
static ArrayOf
int8RowVector(const std::vector<int8>& values);
static ArrayOf
int16RowVector(const std::vector<int16>& values);
static ArrayOf
int32RowVector(const std::vector<int32>& values);
static ArrayOf
int64RowVector(const std::vector<int64>& values);
/**
 * Construct floating-point row vectors.
 */
static ArrayOf
doubleRowVector(indexType len);
static ArrayOf
doubleRowVector(const std::vector<double>& values);
static ArrayOf
doubleComplexRowVector(const std::vector<std::complex<double>>& values);
static ArrayOf
singleRowVector(indexType len);
static ArrayOf
singleComplexRowVector(const std::vector<std::complex<single>>& values);
static ArrayOf
singleRowVector(const std::vector<single>& values);
/**
 * Construct dense two-dimensional numeric matrices.
 */
static ArrayOf
doubleMatrix2d(indexType m, indexType n);
static ArrayOf
singleMatrix2d(indexType m, indexType n);
static ArrayOf
int8Matrix2d(indexType m, indexType n);
static ArrayOf
uint8Matrix2d(indexType m, indexType n);
static ArrayOf
int16Matrix2d(indexType m, indexType n);
static ArrayOf
uint16Matrix2d(indexType m, indexType n);
static ArrayOf
int32Matrix2d(indexType m, indexType n);
static ArrayOf
uint32Matrix2d(indexType m, indexType n);
static ArrayOf
int64Matrix2d(indexType m, indexType n);
static ArrayOf
uint64Matrix2d(indexType m, indexType n);
/**
 * Construct an integer range vector from minval:stepsize:maxval.
 */
static ArrayOf
integerRange(indexType minval, indexType stepsize, indexType maxval, bool vertical);
/**
 * Construct a cell array from rows of ArrayOf values.
 *
 * Each row must have the same number of elements.
 */
static ArrayOf
cellArray(ArrayOfMatrix& m);
/**
 * Return m as a cell array, wrapping non-cell values when needed.
 */
static ArrayOf
cellArrayFrom(ArrayOf m);
/**
 * Build cell arrays whose elements are character vectors.
 */
[[nodiscard]] static ArrayOf
cellArrayFromArrayOfCharacterRowVectors(const stringVector& elements);
[[nodiscard]] static ArrayOf
cellArrayFromArrayOfCharacterRowVectors(const wstringVector& elements);
[[nodiscard]] static ArrayOf
cellArrayFromArrayOfCharacterColumnVectors(const stringVector& elements);
[[nodiscard]] static ArrayOf
cellArrayFromArrayOfCharacterColumnVectors(const wstringVector& elements);
//=============================================================================
