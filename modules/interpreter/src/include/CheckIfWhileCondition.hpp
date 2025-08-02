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
#include "nlsInterpreter_exports.h"
#include "ArrayOf.hpp"
//=============================================================================
namespace Nelson {
/**
 * Returns true if real part is all zeros.  For integer types, this is an
 * element-wise test. For complex types and types not managed throws an exceptions.
 */
NLSINTERPRETER_IMPEXP bool
checkIfWhileCondition(const ArrayOf& A);
}
//=============================================================================
