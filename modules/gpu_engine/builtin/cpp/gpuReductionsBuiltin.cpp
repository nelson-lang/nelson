//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "gpuReductionsBuiltin.hpp"
#include "GpuArrayObject.hpp"
#include "GpuKernels.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
static int
reductionDim(const ArrayOf& dimArg)
{
    return (int)dimArg.asScalarIndex(false);
}
//=============================================================================
#define GPU_REDUCTION_BUILTIN(BUILTIN_NAME, KERNEL_FN)                                             \
    ArrayOfVector Nelson::GpuEngineGateway::BUILTIN_NAME(                                          \
        Evaluator* eval, int nLhs, const ArrayOfVector& argIn)                                     \
    {                                                                                              \
        nargincheck(argIn, 1, 2);                                                                  \
        nargoutcheck(nLhs, 0, 1);                                                                  \
        GpuArrayObject* a = AsGpuArrayObject(argIn[0]);                                            \
        if (a == nullptr) {                                                                        \
            Error(L"Nelson:gpuArray:notGpuArray", L"gpuArray expected.");                          \
        }                                                                                          \
        int dim = (argIn.size() == 2) ? reductionDim(argIn[1]) : -1;                               \
        ArrayOfVector retval;                                                                      \
        retval << KERNEL_FN(a, dim);                                                               \
        return retval;                                                                             \
    }
//=============================================================================
GPU_REDUCTION_BUILTIN(gpuArraySumBuiltin, GpuSum)
GPU_REDUCTION_BUILTIN(gpuArrayProdBuiltin, GpuProd)
GPU_REDUCTION_BUILTIN(gpuArrayMeanBuiltin, GpuMean)
GPU_REDUCTION_BUILTIN(gpuArrayAnyBuiltin, GpuAny)
GPU_REDUCTION_BUILTIN(gpuArrayAllBuiltin, GpuAll)
GPU_REDUCTION_BUILTIN(gpuArrayCumsumBuiltin, GpuCumsum)
GPU_REDUCTION_BUILTIN(gpuArrayCumprodBuiltin, GpuCumprod)
//=============================================================================
// var/std: f(X), f(X, w), f(X, w, dim). w == 1 normalizes by N, otherwise by
// N-1 (the default). An empty w keeps the default.
static ArrayOf
varStdBuiltin(const ArrayOfVector& argIn, ArrayOf (*kernel)(GpuArrayObject*, int, bool))
{
    GpuArrayObject* a = AsGpuArrayObject(argIn[0]);
    if (a == nullptr) {
        Error(L"Nelson:gpuArray:notGpuArray", L"gpuArray expected.");
    }
    bool normalizeByN = false;
    if (argIn.size() >= 2 && !argIn[1].isEmpty()) {
        normalizeByN = (argIn[1].asDoubleScalar() == 1.0);
    }
    int dim = (argIn.size() >= 3) ? (int)argIn[2].asScalarIndex(false) : -1;
    return kernel(a, dim, normalizeByN);
}
//=============================================================================
ArrayOfVector
Nelson::GpuEngineGateway::gpuArrayVarBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 3);
    nargoutcheck(nLhs, 0, 1);
    ArrayOfVector retval;
    retval << varStdBuiltin(argIn, GpuVar);
    return retval;
}
//=============================================================================
ArrayOfVector
Nelson::GpuEngineGateway::gpuArrayStdBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 3);
    nargoutcheck(nLhs, 0, 1);
    ArrayOfVector retval;
    retval << varStdBuiltin(argIn, GpuStd);
    return retval;
}
//=============================================================================
