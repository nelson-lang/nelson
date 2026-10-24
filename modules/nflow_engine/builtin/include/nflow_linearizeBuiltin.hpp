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
//=============================================================================
namespace Nelson::NFlowGateway {
//=============================================================================
// __nflow_linearize__(diagramJson) -> resultJson
// Headless numerical linearization: central-differences the global RHS the
// solver assembles to return the state Jacobians A = df/dx and C = dy/dx about
// the post-INIT operating point. Result JSON: {status, nx, ny, A, C}.
ArrayOfVector
nflow_linearizeBuiltin(int nLhs, const ArrayOfVector& argIn);
//=============================================================================
} // namespace Nelson::NFlowGateway
//=============================================================================
