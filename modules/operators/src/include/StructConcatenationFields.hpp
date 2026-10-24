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
#include "nlsOperators_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
// Structs concatenate when they have the same set of fields, in any order: the
// struct 'value' with its fields in the order of 'fieldnames'. Raises
// Nelson:catenate:structFieldBad when the sets differ.
NLSOPERATORS_IMPEXP ArrayOf
matchStructFields(const ArrayOf& value, const stringVector& fieldnames);
//=============================================================================
} // namespace Nelson
//=============================================================================
