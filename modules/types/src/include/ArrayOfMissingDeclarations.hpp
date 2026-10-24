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
// Missing object
//=============================================================================
/**
 * Construct a scalar missing value.
 */
static ArrayOf
missingScalar();
/**
 * Return whether the value is a scalar missing value.
 */
bool
isMissing() const;
/**
 * Return whether the value is a non-scalar missing array.
 */
bool
isNdArrayMissing() const;
/**
 * Prepare the right-hand side of an indexed assignment into this array when
 * missing is involved: missing becomes NaN in a floating-point destination and
 * stays for a string destination (converted later); any other destination
 * class, or a non-missing value assigned into a missing array, is an error.
 */
void
convertMissingAssignmentValue(ArrayOf& rightData) const;
//=============================================================================
