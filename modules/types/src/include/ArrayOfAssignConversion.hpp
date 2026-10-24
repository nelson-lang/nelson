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
#include "ArrayOf.hpp"
#include "nlsTypes_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
/**
 * Converts a string array to double (or complex double) values: text that is
 * not a number becomes NaN. The types module cannot parse numbers itself, the
 * module owning that conversion registers it.
 */
using StringToDoubleAssignmentConverter = ArrayOf (*)(const ArrayOf& value);
//=============================================================================
NLSTYPES_IMPEXP void
setStringToDoubleAssignmentConverter(StringToDoubleAssignmentConverter converter);
//=============================================================================
/**
 * Indexed assignment into an existing numeric, char or logical array keeps the
 * class of the destination: the right-hand side is converted to it (integers
 * round and saturate, logical rejects NaN and complex, char rejects logical and
 * complex, string text is parsed for double). A complex right-hand side makes a
 * double or single destination complex.
 *
 * Returns false, leaving both values untouched, when the rule does not apply:
 * same class, a destination or value of another kind, or an empty double
 * destination (it adopts the class of the value).
 */
bool
convertAssignmentValueToDestinationClass(ArrayOf& destination, ArrayOf& rightData);
//=============================================================================
} // namespace Nelson
//=============================================================================
