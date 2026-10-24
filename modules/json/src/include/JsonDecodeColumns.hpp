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
#include "nlsJson_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
// Where the table node is, with the rules applied to the jsondecode result
// (keys are matched against the jsondecode field names: the key itself,
// then its valid field name).
enum class JsonTableLocation
{
    Root, // the document itself
    Pointer, // keys: reference tokens of a JSON Pointer through objects
    NodeName, // keys[0]: depth-first first object holding this key
    Automatic // a root array, else depth-first first array of at least two
              // objects, else the root object
};
//=============================================================================
// Decodes a JSON file whose table node is an array of flat objects (every
// value is a number, a boolean, a string or null) directly into columns,
// one per key (field names as given by jsondecode, first appearance order).
// Each column is n x 1:
//   - double: numbers and booleans, at least one number, no null or absent key
//   - logical: booleans only
//   - string: strings only
//   - cell: anything else, with the jsondecode value of each row ([] for null
//     or an absent key)
// selector: JSON Pointer of the table node, made of jsondecode field names.
// Returns false, without error, for any other document (nested values,
// invalid or lenient JSON, node not found or ambiguous, ...): the caller
// decodes it with jsondecode instead.
NLSJSON_IMPEXP bool
jsonDecodeColumnsFile(const std::wstring& filename, JsonTableLocation location,
    const stringVector& keys, std::string& selector, stringVector& names, ArrayOfVector& columns);
//=============================================================================
} // namespace Nelson
//=============================================================================
