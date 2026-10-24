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
#include "nlsLinear_algebra_builtin_exports.h"
#include "NelsonGateway.hpp"
//=============================================================================
namespace Nelson::LinearAlgebraGateway {
//=============================================================================
NLSLINEAR_ALGEBRA_BUILTIN_IMPEXP ArrayOfVector
eigsBuiltin(int nLhs, const ArrayOfVector& argIn);
//=============================================================================
} // namespace Nelson::LinearAlgebraGateway
//=============================================================================
