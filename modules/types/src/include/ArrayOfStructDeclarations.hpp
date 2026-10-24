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
// Shared structure, class, and classdef helpers.
//=============================================================================
/**
 * Return one field from a scalar structure-like value.
 */
[[nodiscard]] ArrayOf
field(const std::string& fieldName) const;
/**
 * Add a field name and return its field index.
 */
indexType
insertFieldName(const std::string& fieldName);
/**
 * Return one field across a structure-like array as a value list.
 */
ArrayOfVector
fieldAsList(const std::string& fieldName) const;
/**
 * Return the field index for a name, or -1 when it is not present.
 */
[[nodiscard]] int64
fieldIndex(const std::string& fieldName) const;
[[nodiscard]] int64
fieldIndexFromList(const std::string& fName, const stringVector& fieldNames) const;
/**
 * Return whether fieldName exists in this structure-like value.
 */
[[nodiscard]] bool
isField(const std::string& fieldName) const;
//=============================================================================
// struct array
//=============================================================================
/**
 * Construct a structure array from field names and values.
 *
 * Scalar values are expanded to match non-scalar cell values. Non-scalar
 * values must agree on their dimensions.
 */
static ArrayOf
structArray(const stringVector& fNames, const ArrayOfVector& values);
static ArrayOf
structArray(const wstringVector& fNames, const ArrayOfVector& values);
/**
 * Construct empty structure arrays.
 */
static ArrayOf
emptyStruct();
static ArrayOf
emptyStruct(const stringVector& fNames, Dimensions& dim);
static ArrayOf
emptyStruct(const wstringVector& fNames, Dimensions& dim);
/**
 * Construct scalar structure values.
 */
static ArrayOf
structScalar(const stringVector& fNames, const ArrayOfVector& values);
static ArrayOf
structScalar(const wstringVector& fNames, const ArrayOfVector& values);
[[nodiscard]] bool
isStruct() const;
/**
 * Replace field names while preserving field storage order.
 */
bool
renameFieldnames(const stringVector& newFieldnames);
//=============================================================================
