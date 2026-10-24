//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "gpuShapeBuiltin.hpp"
#include "GpuKernels.hpp"
#include "GpuArrayObject.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
static GpuArrayObject*
asGpu(const ArrayOf& value);
//=============================================================================
ArrayOfVector
Nelson::GpuEngineGateway::gpuArrayFlipBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 2);
    nargoutcheck(nLhs, 0, 1);
    int dim = (argIn.size() == 2) ? (int)argIn[1].asScalarIndex(false) : -1;
    ArrayOfVector retval;
    retval << GpuFlip(asGpu(argIn[0]), dim);
    return retval;
}
//=============================================================================
ArrayOfVector
Nelson::GpuEngineGateway::gpuArrayCircshiftBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 2, 3);
    nargoutcheck(nLhs, 0, 1);
    int shift = (int)argIn[1].asDoubleScalar();
    int dim = (argIn.size() == 3) ? (int)argIn[2].asScalarIndex(false) : -1;
    ArrayOfVector retval;
    retval << GpuCircshift(asGpu(argIn[0]), shift, dim);
    return retval;
}
//=============================================================================
ArrayOfVector
Nelson::GpuEngineGateway::gpuArrayRepmatBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 2, 3);
    nargoutcheck(nLhs, 0, 1);
    indexType rowCopies = 1;
    indexType colCopies = 1;
    if (argIn.size() == 2) {
        ArrayOf rep = argIn[1];
        if (rep.isScalar()) {
            rowCopies = rep.asScalarIndex(false);
            colCopies = rowCopies;
        } else if (rep.isRowVector() && rep.elementCount() == 2) {
            ArrayOf r(rep);
            const indexType* p = (const indexType*)r.indexPointer();
            rowCopies = p[0];
            colCopies = p[1];
        } else {
            Error(L"Nelson:repmat:invalidReplicationFactor",
                L"repmat: replication factor must be a scalar or a 2-element vector.");
        }
    } else {
        rowCopies = argIn[1].asScalarIndex(false);
        colCopies = argIn[2].asScalarIndex(false);
    }
    ArrayOfVector retval;
    retval << GpuRepmat(asGpu(argIn[0]), rowCopies, colCopies);
    return retval;
}
//=============================================================================
ArrayOfVector
Nelson::GpuEngineGateway::gpuArrayDiffBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 3);
    nargoutcheck(nLhs, 0, 1);
    int order = (argIn.size() >= 2 && !argIn[1].isEmpty()) ? (int)argIn[1].asScalarIndex(false) : 1;
    int dim = (argIn.size() == 3) ? (int)argIn[2].asScalarIndex(false) : -1;
    ArrayOfVector retval;
    retval << GpuDiff(asGpu(argIn[0]), order, dim);
    return retval;
}
//=============================================================================
ArrayOfVector
Nelson::GpuEngineGateway::gpuArrayKronBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 2, 2);
    nargoutcheck(nLhs, 0, 1);
    ArrayOfVector retval;
    retval << GpuKron(asGpu(argIn[0]), asGpu(argIn[1]));
    return retval;
}
//=============================================================================
ArrayOfVector
Nelson::GpuEngineGateway::gpuArrayTraceBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 1);
    ArrayOfVector retval;
    retval << GpuTrace(asGpu(argIn[0]));
    return retval;
}
//=============================================================================
ArrayOfVector
Nelson::GpuEngineGateway::gpuArrayTriuBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 2);
    nargoutcheck(nLhs, 0, 1);
    int k = (argIn.size() == 2) ? (int)argIn[1].asDoubleScalar() : 0;
    ArrayOfVector retval;
    retval << GpuTriangular(asGpu(argIn[0]), k, true);
    return retval;
}
//=============================================================================
ArrayOfVector
Nelson::GpuEngineGateway::gpuArrayTrilBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 2);
    nargoutcheck(nLhs, 0, 1);
    int k = (argIn.size() == 2) ? (int)argIn[1].asDoubleScalar() : 0;
    ArrayOfVector retval;
    retval << GpuTriangular(asGpu(argIn[0]), k, false);
    return retval;
}
//=============================================================================
ArrayOfVector
Nelson::GpuEngineGateway::gpuArrayCummaxBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 2);
    nargoutcheck(nLhs, 0, 1);
    int dim = (argIn.size() == 2) ? (int)argIn[1].asScalarIndex(false) : -1;
    ArrayOfVector retval;
    retval << GpuCummax(asGpu(argIn[0]), dim);
    return retval;
}
//=============================================================================
ArrayOfVector
Nelson::GpuEngineGateway::gpuArrayCumminBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 2);
    nargoutcheck(nLhs, 0, 1);
    int dim = (argIn.size() == 2) ? (int)argIn[1].asScalarIndex(false) : -1;
    ArrayOfVector retval;
    retval << GpuCummin(asGpu(argIn[0]), dim);
    return retval;
}
//=============================================================================
// static helper functions
//=============================================================================
static GpuArrayObject*
asGpu(const ArrayOf& value)
{
    GpuArrayObject* object = AsGpuArrayObject(value);
    if (object == nullptr) {
        Error(L"Nelson:gpuArray:notGpuArray", L"gpuArray expected.");
    }
    return object;
}
//=============================================================================
