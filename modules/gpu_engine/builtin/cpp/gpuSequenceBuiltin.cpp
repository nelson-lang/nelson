//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <cmath>
#include "gpuSequenceBuiltin.hpp"
#include "GpuKernels.hpp"
#include "GpuArrayObject.hpp"
#include "GpuTransfer.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
static double
scalarValue(const ArrayOf& value);
//=============================================================================
ArrayOfVector
Nelson::GpuEngineGateway::gpuArrayLinspaceBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 2, 3);
    nargoutcheck(nLhs, 0, 1);
    double a = scalarValue(argIn[0]);
    double b = scalarValue(argIn[1]);
    indexType n = (argIn.size() == 3) ? (indexType)scalarValue(argIn[2]) : 100;
    if (n < 0) {
        n = 0;
    }
    ArrayOfVector retval;
    if (n == 0) {
        retval << GpuAffineSequence(0, 0.0f, 0.0f, false, 0.0f);
    } else if (n == 1) {
        retval << GpuAffineSequence(1, (float)b, 0.0f, true, (float)b);
    } else {
        float inc = (float)((b - a) / (double)(n - 1));
        retval << GpuAffineSequence(n, (float)a, inc, true, (float)b);
    }
    return retval;
}
//=============================================================================
ArrayOfVector
Nelson::GpuEngineGateway::gpuArrayColonBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 2, 3);
    nargoutcheck(nLhs, 0, 1);
    double a = scalarValue(argIn[0]);
    double step = (argIn.size() == 3) ? scalarValue(argIn[1]) : 1.0;
    double b = (argIn.size() == 3) ? scalarValue(argIn[2]) : scalarValue(argIn[1]);
    if (step == 0.0) {
        Error(L"Nelson:colon:zeroStep", L"colon: step must be non-zero.");
    }
    double count = (b - a) / step;
    indexType n = (count < -1e-10) ? 0 : (indexType)(std::floor(count + 1e-10) + 1);
    ArrayOfVector retval;
    retval << GpuAffineSequence(n, (float)a, (float)step, false, 0.0f);
    return retval;
}
//=============================================================================
// static helper functions
//=============================================================================
static double
scalarValue(const ArrayOf& value)
{
    GpuArrayObject* object = AsGpuArrayObject(value);
    if (object != nullptr) {
        return GatherGpuArray(object).asDoubleScalar();
    }
    return value.asDoubleScalar();
}
//=============================================================================
