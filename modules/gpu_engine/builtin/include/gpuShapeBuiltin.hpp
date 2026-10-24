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
ArrayOfVector
gpuArrayFlipBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
ArrayOfVector
gpuArrayCircshiftBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
ArrayOfVector
gpuArrayRepmatBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
ArrayOfVector
gpuArrayDiffBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
ArrayOfVector
gpuArrayKronBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
ArrayOfVector
gpuArrayTraceBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
ArrayOfVector
gpuArrayTriuBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
ArrayOfVector
gpuArrayTrilBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
ArrayOfVector
gpuArrayCummaxBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
ArrayOfVector
gpuArrayCumminBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
//=============================================================================
} // namespace Nelson::GpuEngineGateway
//=============================================================================
