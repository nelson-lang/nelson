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
namespace Nelson {
class Evaluator;
//=============================================================================
} // namespace Nelson
//=============================================================================
namespace Nelson::NFlowGateway {
//=============================================================================
// __nflow_simulate__(diagramJson) -> resultJson
// Headless simulation runner: runs the engine on a .nflow diagram JSON string and
// returns the result JSON, with no GUI. Needs the Evaluator to install the
// workspace bridge (nelsonFunction, fromWorkspace, toWorkspace blocks).
ArrayOfVector
nflow_simulateBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
//=============================================================================
} // namespace Nelson
//=============================================================================
