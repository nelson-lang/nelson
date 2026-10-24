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
// Reductions and scans along a dimension, plus the variance/standard deviation.
ArrayOfVector
gpuArraySumBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
ArrayOfVector
gpuArrayProdBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
ArrayOfVector
gpuArrayMeanBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
ArrayOfVector
gpuArrayAnyBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
ArrayOfVector
gpuArrayAllBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
ArrayOfVector
gpuArrayCumsumBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
ArrayOfVector
gpuArrayCumprodBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
ArrayOfVector
gpuArrayVarBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
ArrayOfVector
gpuArrayStdBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
//=============================================================================
} // namespace Nelson::GpuEngineGateway
//=============================================================================
