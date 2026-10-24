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
gpuArrayPlusBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
ArrayOfVector
gpuArrayMinusBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
ArrayOfVector
gpuArrayTimesBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
ArrayOfVector
gpuArrayRdivideBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
ArrayOfVector
gpuArrayLdivideBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
ArrayOfVector
gpuArrayPowerBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
ArrayOfVector
gpuArrayMtimesBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
//=============================================================================
ArrayOfVector
gpuArrayRealBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
ArrayOfVector
gpuArrayImagBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
ArrayOfVector
gpuArrayConjBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
ArrayOfVector
gpuArrayAngleBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
ArrayOfVector
gpuArrayIsrealBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
//=============================================================================
ArrayOfVector
gpuArrayUminusBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
ArrayOfVector
gpuArrayAbsBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
ArrayOfVector
gpuArraySqrtBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
ArrayOfVector
gpuArrayExpBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
ArrayOfVector
gpuArrayLogBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
ArrayOfVector
gpuArraySinBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
ArrayOfVector
gpuArrayCosBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
ArrayOfVector
gpuArrayTanBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
ArrayOfVector
gpuArrayFloorBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
ArrayOfVector
gpuArrayCeilBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
ArrayOfVector
gpuArrayRoundBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
ArrayOfVector
gpuArrayFixBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
ArrayOfVector
gpuArraySignBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
ArrayOfVector
gpuArrayModBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
ArrayOfVector
gpuArrayRemBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
ArrayOfVector
gpuArraySinhBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
ArrayOfVector
gpuArrayCoshBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
ArrayOfVector
gpuArrayTanhBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
ArrayOfVector
gpuArrayAsinBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
ArrayOfVector
gpuArrayAcosBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
ArrayOfVector
gpuArrayAtanBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
ArrayOfVector
gpuArrayLog2Builtin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
ArrayOfVector
gpuArrayLog10Builtin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
ArrayOfVector
gpuArrayAtan2Builtin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
ArrayOfVector
gpuArrayHypotBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
ArrayOfVector
gpuArrayExpm1Builtin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
ArrayOfVector
gpuArrayLog1pBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
ArrayOfVector
gpuArrayPow2Builtin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
ArrayOfVector
gpuArraySecBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
ArrayOfVector
gpuArrayCscBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
ArrayOfVector
gpuArrayCotBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
ArrayOfVector
gpuArrayAsecBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
ArrayOfVector
gpuArrayAcscBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
ArrayOfVector
gpuArrayAcotBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
ArrayOfVector
gpuArraySechBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
ArrayOfVector
gpuArrayCschBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
ArrayOfVector
gpuArrayCothBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
ArrayOfVector
gpuArrayAsinhBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
ArrayOfVector
gpuArrayAcoshBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
ArrayOfVector
gpuArrayAtanhBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
ArrayOfVector
gpuArrayDeg2radBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
ArrayOfVector
gpuArrayRad2degBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
ArrayOfVector
gpuArrayIsnanBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
ArrayOfVector
gpuArrayIsinfBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
ArrayOfVector
gpuArrayIsfiniteBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
ArrayOfVector
gpuArrayBitandBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
ArrayOfVector
gpuArrayBitorBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
ArrayOfVector
gpuArrayBitxorBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
ArrayOfVector
gpuArrayBitshiftBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
//=============================================================================
// Relational and logical operators (result: logical gpuArray)
ArrayOfVector
gpuArrayEqBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
ArrayOfVector
gpuArrayNeBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
ArrayOfVector
gpuArrayLtBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
ArrayOfVector
gpuArrayLeBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
ArrayOfVector
gpuArrayGtBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
ArrayOfVector
gpuArrayGeBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
ArrayOfVector
gpuArrayAndBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
ArrayOfVector
gpuArrayOrBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
ArrayOfVector
gpuArrayXorBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
ArrayOfVector
gpuArrayNotBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
//=============================================================================
// Reductions returning a single value along a dimension (max/min); the other
// reductions (sum/prod/mean/any/all/cumsum/cumprod/var/std) are declared in
// gpuReductionsBuiltin.hpp.
ArrayOfVector
gpuArrayMaxBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
ArrayOfVector
gpuArrayMinBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
ArrayOfVector
gpuArraySortBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
ArrayOfVector
gpuArrayFindBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
//=============================================================================
ArrayOfVector
gpuArrayTransposeBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
ArrayOfVector
gpuArrayCtransposeBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
ArrayOfVector
gpuArrayReshapeBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
ArrayOfVector
gpuArrayIndexBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
ArrayOfVector
gpuArrayAsgnBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
ArrayOfVector
gpuArrayHorzcatBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
ArrayOfVector
gpuArrayVertcatBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
ArrayOfVector
gpuArrayCatBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
//=============================================================================
// arrayfun (fused kernel). Returns {ok(logical), result}; ok=false signals the
// caller (.m) to fall back to a host arrayfun.
ArrayOfVector
gpuArrayArrayfunBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
//=============================================================================
ArrayOfVector
gpuArraySizeBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
ArrayOfVector
gpuArrayDispBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
//=============================================================================
} // namespace Nelson::GpuEngineGateway
//=============================================================================
