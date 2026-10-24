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
#include "Evaluator.hpp"
//=============================================================================
namespace Nelson::GpuEngineGateway {
//=============================================================================
// Sequence constructors dispatched from a gpuArray endpoint: linspace and the
// colon operator build the row vector directly on the device.
ArrayOfVector
gpuArrayLinspaceBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
ArrayOfVector
gpuArrayColonBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
//=============================================================================
} // namespace Nelson::GpuEngineGateway
//=============================================================================
