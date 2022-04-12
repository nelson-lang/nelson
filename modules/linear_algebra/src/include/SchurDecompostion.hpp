//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include "ArrayOf.hpp"
#include "nlsLinear_algebra_exports.h"
//=============================================================================
namespace Nelson {
NLSLINEAR_ALGEBRA_IMPEXP void
SchurDecomposition(ArrayOf A, bool asComplex, ArrayOf& T);
NLSLINEAR_ALGEBRA_IMPEXP void
SchurDecomposition(ArrayOf A, bool asComplex, ArrayOf& U, ArrayOf& T);
} // namespace Nelson
//=============================================================================
