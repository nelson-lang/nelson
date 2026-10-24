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
// string array
//=============================================================================
/**
 * Return whether the value stores string array data.
 */
[[nodiscard]] bool
isStringArray() const;
/**
 * Return whether the value is a non-scalar string array.
 */
[[nodiscard]] bool
isNdArrayString() const;
/**
 * Return whether the value is a scalar string array.
 *
 * @param withMissing Treat a missing scalar as an accepted string value.
 */
[[nodiscard]] bool
isScalarStringArray(bool withMissing = false) const;
/**
 * Construct a scalar string array from UTF-8 text.
 */
static ArrayOf
stringArray(const std::string& value);
/**
 * Construct a scalar string array from wide text.
 */
static ArrayOf
stringArray(const std::wstring& value);
/**
 * Construct a string array whose elements are all missing.
 */
static ArrayOf
missingStringArray(Dimensions& dims);
/**
 * Construct a string array from UTF-8 elements and dimensions.
 */
static ArrayOf
stringArray(const stringVector& values, const Dimensions& dims);
/**
 * Construct a string array from wide elements and dimensions.
 */
static ArrayOf
stringArray(const wstringVector& values, const Dimensions& dims);
/**
 * Convert a value to a string array when the conversion is direct.
 *
 * needToOverload is set when the caller must dispatch overloaded behavior.
 */
static ArrayOf
toStringArray(const ArrayOf& m, bool& needToOverload);
//=============================================================================
