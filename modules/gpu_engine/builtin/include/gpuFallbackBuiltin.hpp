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
#include "gpuFallbackList.hpp"
#include "GpuArrayObject.hpp"
//=============================================================================
namespace Nelson::GpuEngineGateway {
//=============================================================================
// run the host function `name` on gathered inputs; numeric results go back to the device
ArrayOfVector
gpuArrayHostFallback(Evaluator* eval, const std::wstring& name, int nLhs, const ArrayOfVector& argIn);
//=============================================================================
// device copy of a real double/single (or logical) argument; owner keeps an upload alive
GpuArrayObject*
gpuArrayDeviceOperand(const ArrayOf& value, ArrayOf& owner, bool allowLogical);
//=============================================================================
ArrayOfVector
gpuArrayBetaincBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
//=============================================================================
ArrayOfVector
gpuArrayGammaincBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
//=============================================================================
ArrayOfVector
gpuArrayHostCallBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
//=============================================================================
#define GPU_FALLBACK_DECLARE(NAME)                                                                 \
    ArrayOfVector gpuArrayFallback_##NAME(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
GPU_FALLBACK_FUNCTIONS(GPU_FALLBACK_DECLARE)
#undef GPU_FALLBACK_DECLARE
#define GPU_MATH_DECLARE(NAME)                                                                     \
    ArrayOfVector gpuArrayMath_##NAME(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
GPU_ELEMENTWISE_MATH_FUNCTIONS(GPU_MATH_DECLARE)
#undef GPU_MATH_DECLARE
//=============================================================================
} // namespace Nelson::GpuEngineGateway
//=============================================================================
