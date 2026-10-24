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
// class array
//=============================================================================
/**
 * Return whether the value stores named class data.
 */
[[nodiscard]] bool
isClassType() const;
/**
 * Return whether the value stores classdef data.
 */
[[nodiscard]] bool
isClassdefType() const;
/**
 * Set the class name stored with a structure-like value.
 */
void
setClassType(const std::string& structname);
void
setClassType(const std::wstring& structname);
/**
 * Return the class name stored with this value.
 */
[[nodiscard]] std::string
classType() const;
/**
 * Return classdef layout metadata attached to this value.
 */
[[nodiscard]] ClassdefLayoutPtr
classdefLayout() const;
/**
 * Attach classdef layout metadata to this value.
 */
void
setClassdefLayout(ClassdefLayoutPtr layout);
/**
 * Construct named class values from field names and field values.
 */
static ArrayOf
classArray(
    const std::wstring& classTypeName, const wstringVector& fNames, const ArrayOfVector& values);
static ArrayOf
classArray(
    const std::string& classTypeName, const stringVector& fNames, const ArrayOfVector& values);
/**
 * Construct classdef values from field names and field values.
 */
static ArrayOf
classdefArray(
    const std::string& classTypeName, const stringVector& fNames, const ArrayOfVector& values);
/**
 * Construct empty named class values.
 */
static ArrayOf
emptyClass(const std::wstring& className, const wstringVector& fNames, Dimensions& dim);
static ArrayOf
emptyClass(const std::string& className, const stringVector& fNames, Dimensions& dim);
/**
 * Construct an empty classdef value with the requested fields.
 */
static ArrayOf
emptyClassdef(const std::string& className, const stringVector& fNames, Dimensions& dim);
//=============================================================================
