//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <algorithm>
#include <cmath>
#include "gpuHistogramBuiltin.hpp"
#include "gpuFallbackBuiltin.hpp"
#include "GpuArrayObject.hpp"
#include "GpuHistogram.hpp"
#include "GpuKernels.hpp"
#include "GpuTransfer.hpp"
#include "FunctionDef.hpp"
#include "Error.hpp"
#include "i18n.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
namespace {
//=============================================================================
ArrayOfVector
host(Evaluator* eval, const wchar_t* name, int nLhs, const ArrayOfVector& argIn)
{
    return GpuEngineGateway::gpuArrayHostFallback(eval, name, nLhs, argIn);
}
//=============================================================================
ArrayOfVector
one(const ArrayOf& value)
{
    ArrayOfVector r;
    r << value;
    return r;
}
//=============================================================================
// true when every value is a whole number >= 1
bool
validSubscripts(GpuArrayObject* subs)
{
    if (subs->elementCount() == 0) {
        return true;
    }
    ArrayOf bad = GpuUnaryElementwise(
        subs, "gpuArray:badsubscript", "f32(fract(ai) != 0.0 || !(ai >= 1.0))", "logical");
    GpuArrayObject* b = AsGpuArrayObject(bad);
    ArrayOf column = ArrayOf::handle(GpuArrayObject::view(b, Dimensions(b->elementCount(), 1)));
    ArrayOf any = GatherGpuArray(AsGpuArrayObject(GpuAny(AsGpuArrayObject(column), 1)));
    return !any.asLogicalScalar();
}
//=============================================================================
} // namespace
//=============================================================================
ArrayOfVector
Nelson::GpuEngineGateway::gpuArrayHistcountsBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOf ownerX, ownerE, result;
    if (argIn.size() == 2 && nLhs <= 1 && (argIn[1].isVector() || AsGpuArrayObject(argIn[1]))
        && !argIn[1].isScalar()) {
        GpuArrayObject* x = GpuEngineGateway::gpuArrayDeviceOperand(argIn[0], ownerX, false);
        GpuArrayObject* e = GpuEngineGateway::gpuArrayDeviceOperand(argIn[1], ownerE, false);
        if (x && e && e->elementCount() >= 2 && GpuHistcounts(x, e, result)) {
            return one(result);
        }
    }
    return host(eval, L"histcounts", nLhs, argIn);
}
//=============================================================================
ArrayOfVector
Nelson::GpuEngineGateway::gpuArrayDiscretizeBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOf ownerX, ownerE, result;
    if (argIn.size() == 2 && nLhs <= 1) {
        GpuArrayObject* x = GpuEngineGateway::gpuArrayDeviceOperand(argIn[0], ownerX, false);
        GpuArrayObject* e = GpuEngineGateway::gpuArrayDeviceOperand(argIn[1], ownerE, false);
        if (x && e && e->elementCount() >= 2 && GpuDiscretize(x, e, result)) {
            return one(result);
        }
    }
    return host(eval, L"discretize", nLhs, argIn);
}
//=============================================================================
// accumarray(subs, vals [, sz [, fun [, fillval]]]) with sum, max or min
ArrayOfVector
Nelson::GpuEngineGateway::gpuArrayAccumarrayBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    if (argIn.size() < 2 || argIn.size() > 5 || nLhs > 1) {
        return host(eval, L"accumarray", nLhs, argIn);
    }
    ArrayOf ownerS, ownerV;
    GpuArrayObject* subs = GpuEngineGateway::gpuArrayDeviceOperand(argIn[0], ownerS, false);
    GpuArrayObject* vals = GpuEngineGateway::gpuArrayDeviceOperand(argIn[1], ownerV, false);
    if (subs == nullptr || vals == nullptr || subs->dimensions().length() > 2
        || !validSubscripts(subs)) {
        return host(eval, L"accumarray", nLhs, argIn);
    }
    indexType columns = subs->dimensions().columns();
    std::vector<indexType> outDims;
    if (argIn.size() >= 3 && !argIn[2].isEmpty()) {
        ArrayOf sz = AsGpuArrayObject(argIn[2]) ? GatherGpuArray(AsGpuArrayObject(argIn[2])) : argIn[2];
        if (!sz.isNumeric() || !sz.isVector()) {
            return host(eval, L"accumarray", nLhs, argIn);
        }
        sz.promoteType(NLS_DOUBLE);
        const double* p = (const double*)sz.dataPointer();
        for (indexType k = 0; k < sz.elementCount(); ++k) {
            outDims.push_back((indexType)p[k]);
        }
    }
    if (outDims.empty() || (indexType)outDims.size() < columns) {
        // size from the largest subscript of every column
        ArrayOf largest = GatherGpuArray(AsGpuArrayObject(GpuMaxAlong(subs, 1, false)));
        largest.promoteType(NLS_DOUBLE);
        const double* p = (const double*)largest.dataPointer();
        bool given = !outDims.empty();
        std::vector<indexType> computed;
        for (indexType k = 0; k < largest.elementCount(); ++k) {
            computed.push_back(subs->elementCount() ? (indexType)p[k] : 0);
        }
        if (given) {
            return host(eval, L"accumarray", nLhs, argIn);
        }
        outDims = computed;
        if (outDims.size() == 1) {
            outDims.push_back(1);
        }
    } else {
        // subscripts beyond the requested size are an error the host reports
        ArrayOf largest = GatherGpuArray(AsGpuArrayObject(GpuMaxAlong(subs, 1, false)));
        largest.promoteType(NLS_DOUBLE);
        const double* p = (const double*)largest.dataPointer();
        for (indexType k = 0; k < largest.elementCount(); ++k) {
            if (subs->elementCount() && p[k] > (double)outDims[(size_t)k]) {
                return host(eval, L"accumarray", nLhs, argIn);
            }
        }
    }
    int op = 0;
    if (argIn.size() >= 4 && !argIn[3].isEmpty()) {
        if (!argIn[3].isFunctionHandle()) {
            return host(eval, L"accumarray", nLhs, argIn);
        }
        FunctionDef* f = reinterpret_cast<FunctionDef*>(argIn[3].asFunctionHandle().anonymousHandle);
        std::string name = f ? f->getName() : std::string();
        if (name == "sum") {
            op = 0;
        } else if (name == "max") {
            op = 1;
        } else if (name == "min") {
            op = 2;
        } else {
            return host(eval, L"accumarray", nLhs, argIn);
        }
    }
    float fill = 0.0f;
    if (argIn.size() == 5) {
        if (AsGpuArrayObject(argIn[4]) || !argIn[4].isNumeric() || !argIn[4].isScalar()
            || argIn[4].isComplex()) {
            return host(eval, L"accumarray", nLhs, argIn);
        }
        fill = (float)argIn[4].asDoubleScalar();
    }
    ArrayOf result;
    if (!GpuAccumarray(subs, vals, outDims, op, fill, result)) {
        return host(eval, L"accumarray", nLhs, argIn);
    }
    return one(result);
}
//=============================================================================
