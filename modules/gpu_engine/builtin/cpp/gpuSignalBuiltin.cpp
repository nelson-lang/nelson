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
#include "gpuSignalBuiltin.hpp"
#include "gpuFallbackBuiltin.hpp"
#include "GpuArrayObject.hpp"
#include "GpuKernels.hpp"
#include "GpuSignal.hpp"
#include "GpuTransfer.hpp"
#include "Error.hpp"
#include "i18n.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
namespace {
//=============================================================================
bool
text(const ArrayOf& value, std::string& out)
{
    if (!(value.isRowVectorCharacterArray() || value.isScalarStringArray())) {
        return false;
    }
    out = value.asUtf8String();
    std::transform(out.begin(), out.end(), out.begin(), ::tolower);
    return true;
}
//=============================================================================
// small host or device vector as floats
bool
hostValues(const ArrayOf& value, std::vector<float>& out)
{
    GpuArrayObject* g = AsGpuArrayObject(value);
    ArrayOf h = g ? GatherGpuArray(g) : value;
    if (!(h.isNumeric() || h.isLogical()) || h.isComplex() || h.isSparse()
        || !(h.isVector() || h.isScalar())) {
        return false;
    }
    h.promoteType(NLS_SINGLE);
    const single* p = (const single*)h.dataPointer();
    out.assign(p, p + h.elementCount());
    return true;
}
//=============================================================================
bool
scalarInt(const ArrayOf& value, int& out)
{
    if (AsGpuArrayObject(value) || !value.isNumeric() || !value.isScalar() || value.isComplex()) {
        return false;
    }
    double d = value.asDoubleScalar();
    if (d < 1 || d != (double)(int)d) {
        return false;
    }
    out = (int)d;
    return true;
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
ArrayOfVector
host(Evaluator* eval, const wchar_t* name, int nLhs, const ArrayOfVector& argIn)
{
    return GpuEngineGateway::gpuArrayHostFallback(eval, name, nLhs, argIn);
}
//=============================================================================
ArrayOfVector
trapzLike(Evaluator* eval, int nLhs, const ArrayOfVector& argIn, bool cumulative)
{
    const wchar_t* name = cumulative ? L"cumtrapz" : L"trapz";
    if (argIn.empty() || argIn.size() > 3 || nLhs > 1) {
        return host(eval, name, nLhs, argIn);
    }
    // (y), (y, dim), (spacing, y), (spacing, y, dim)
    float spacing = 1.0f;
    int dim = -1;
    size_t yIndex = 0;
    if (argIn.size() == 2) {
        if (scalarInt(argIn[1], dim) && AsGpuArrayObject(argIn[1]) == nullptr
            && argIn[1].isScalar()) {
            yIndex = 0;
        } else {
            yIndex = 1;
            dim = -1;
        }
    } else if (argIn.size() == 3) {
        yIndex = 1;
        if (!scalarInt(argIn[2], dim)) {
            return host(eval, name, nLhs, argIn);
        }
    }
    if (yIndex == 1) {
        std::vector<float> x;
        if (!hostValues(argIn[0], x) || x.size() != 1) {
            return host(eval, name, nLhs, argIn);
        }
        spacing = x[0];
    }
    ArrayOf owner;
    GpuArrayObject* y = GpuEngineGateway::gpuArrayDeviceOperand(argIn[yIndex], owner, true);
    ArrayOf result;
    if (y == nullptr || !GpuTrapz(y, spacing, dim, cumulative, result)) {
        return host(eval, name, nLhs, argIn);
    }
    return one(result);
}
//=============================================================================
// movXXX(A, k [, w] [, dim] [, nanflag]); name-value options stay on the host
ArrayOfVector
moving(Evaluator* eval, int nLhs, const ArrayOfVector& argIn, const std::string& op)
{
    std::wstring wname = L"mov" + std::wstring(op.begin(), op.end());
    bool hasWeight = (op == "var" || op == "std");
    GpuArrayObject* x = AsGpuArrayObject(argIn.empty() ? ArrayOf() : argIn[0]);
    if (x == nullptr || argIn.size() < 2 || nLhs > 1) {
        return host(eval, wname.c_str(), nLhs, argIn);
    }
    std::vector<float> k;
    if (!hostValues(argIn[1], k) || k.empty() || k.size() > 2) {
        return host(eval, wname.c_str(), nLhs, argIn);
    }
    uint32_t before = 0;
    uint32_t after = 0;
    if (k.size() == 1) {
        if (k[0] < 1 || k[0] != (float)(int)k[0]) {
            return host(eval, wname.c_str(), nLhs, argIn);
        }
        uint32_t w = (uint32_t)k[0];
        before = w / 2;
        after = (w % 2 == 1) ? w / 2 : w / 2 - 1;
    } else {
        if (k[0] < 0 || k[1] < 0 || k[0] != (float)(int)k[0] || k[1] != (float)(int)k[1]) {
            return host(eval, wname.c_str(), nLhs, argIn);
        }
        before = (uint32_t)k[0];
        after = (uint32_t)k[1];
    }
    bool omitNan = (op == "max" || op == "min");
    bool normalizeByN = false;
    int dim = -1;
    size_t next = 2;
    if (hasWeight && next < argIn.size() && !argIn[next].isRowVectorCharacterArray()
        && !argIn[next].isScalarStringArray()) {
        std::vector<float> w;
        if (!hostValues(argIn[next], w) || (w.size() > 1)
            || (w.size() == 1 && w[0] != 0 && w[0] != 1)) {
            return host(eval, wname.c_str(), nLhs, argIn);
        }
        normalizeByN = w.size() == 1 && w[0] == 1;
        next++;
    }
    if (next < argIn.size() && !argIn[next].isRowVectorCharacterArray()
        && !argIn[next].isScalarStringArray()) {
        if (!scalarInt(argIn[next], dim)) {
            return host(eval, wname.c_str(), nLhs, argIn);
        }
        next++;
    }
    if (next < argIn.size()) {
        std::string flag;
        if (!text(argIn[next], flag) || next + 1 != argIn.size()) {
            return host(eval, wname.c_str(), nLhs, argIn);
        }
        if (flag == "omitnan" || flag == "omitmissing") {
            omitNan = true;
        } else if ((flag == "includenan" || flag == "includemissing") && op != "max"
            && op != "min") {
            omitNan = false;
        } else {
            return host(eval, wname.c_str(), nLhs, argIn);
        }
    }
    ArrayOf result;
    if (!GpuMoving(x, op, before, after, dim, omitNan, normalizeByN, result)) {
        return host(eval, wname.c_str(), nLhs, argIn);
    }
    return one(result);
}
//=============================================================================
} // namespace
//=============================================================================
#define GPU_MOVING_BUILTIN(NAME, OP)                                                               \
    ArrayOfVector Nelson::GpuEngineGateway::NAME(                                                  \
        Evaluator* eval, int nLhs, const ArrayOfVector& argIn)                                     \
    {                                                                                              \
        return moving(eval, nLhs, argIn, OP);                                                      \
    }
GPU_MOVING_BUILTIN(gpuArrayMovsumBuiltin, "sum")
GPU_MOVING_BUILTIN(gpuArrayMovmeanBuiltin, "mean")
GPU_MOVING_BUILTIN(gpuArrayMovprodBuiltin, "prod")
GPU_MOVING_BUILTIN(gpuArrayMovmaxBuiltin, "max")
GPU_MOVING_BUILTIN(gpuArrayMovminBuiltin, "min")
GPU_MOVING_BUILTIN(gpuArrayMovvarBuiltin, "var")
GPU_MOVING_BUILTIN(gpuArrayMovstdBuiltin, "std")
#undef GPU_MOVING_BUILTIN
//=============================================================================
ArrayOfVector
Nelson::GpuEngineGateway::gpuArrayPolyvalBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOf ownerP, ownerX;
    ArrayOf result;
    if (argIn.size() == 2 && nLhs <= 1) {
        GpuArrayObject* p = GpuEngineGateway::gpuArrayDeviceOperand(argIn[0], ownerP, true);
        GpuArrayObject* x = GpuEngineGateway::gpuArrayDeviceOperand(argIn[1], ownerX, true);
        if (p && x && GpuPolyval(p, x, result)) {
            return one(result);
        }
    }
    return host(eval, L"polyval", nLhs, argIn);
}
//=============================================================================
ArrayOfVector
Nelson::GpuEngineGateway::gpuArrayConvBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    std::string shape = "full";
    if ((argIn.size() == 2 || (argIn.size() == 3 && text(argIn[2], shape))) && nLhs <= 1) {
        ArrayOf ownerU, ownerV, result;
        GpuArrayObject* u = GpuEngineGateway::gpuArrayDeviceOperand(argIn[0], ownerU, true);
        GpuArrayObject* v = GpuEngineGateway::gpuArrayDeviceOperand(argIn[1], ownerV, true);
        if (u && v && GpuConv(u, v, shape, result)) {
            return one(result);
        }
    }
    return host(eval, L"conv", nLhs, argIn);
}
//=============================================================================
// conv2(A, B [, shape]) and conv2(u, v, A [, shape]) = conv2(A, u(:) * v(:).' [, shape])
ArrayOfVector
Nelson::GpuEngineGateway::gpuArrayConv2Builtin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    std::string shape = "full";
    size_t count = argIn.size();
    if (count >= 3 && text(argIn[count - 1], shape)) {
        count--;
    }
    if (nLhs <= 1 && (count == 2 || count == 3)) {
        ArrayOf owner1, owner2, owner3, kernelOwner, result;
        GpuArrayObject* first = GpuEngineGateway::gpuArrayDeviceOperand(argIn[0], owner1, true);
        GpuArrayObject* second = GpuEngineGateway::gpuArrayDeviceOperand(argIn[1], owner2, true);
        GpuArrayObject* data = first;
        GpuArrayObject* kernel = second;
        if (count == 3 && first && second
            && (first->dimensions().isVector() || first->dimensions().isScalar())
            && (second->dimensions().isVector() || second->dimensions().isScalar())) {
            data = GpuEngineGateway::gpuArrayDeviceOperand(argIn[2], owner3, true);
            ArrayOf column = ArrayOf::handle(
                GpuArrayObject::view(first, Dimensions(first->elementCount(), 1)));
            ArrayOf row = ArrayOf::handle(
                GpuArrayObject::view(second, Dimensions(1, second->elementCount())));
            kernelOwner = GpuMtimes(AsGpuArrayObject(column), AsGpuArrayObject(row));
            kernel = AsGpuArrayObject(kernelOwner);
        } else if (count == 3) {
            data = nullptr;
        }
        if (data && kernel && GpuConv2(data, kernel, shape, result)) {
            return one(result);
        }
    }
    return host(eval, L"conv2", nLhs, argIn);
}
//=============================================================================
// grid interpolation shared by interp2, interp3 and interpn: (grids..., V, queries...) or
// (V, queries...), then an optional method and fill value; meshgrid order swaps the first two
static bool
gridInterpolation(const ArrayOfVector& argIn, size_t rank, bool meshgridOrder, ArrayOf& result)
{
    size_t n = argIn.size();
    // trailing method and fill value
    size_t data = n;
    std::string method = "linear";
    std::vector<float> fill;
    bool useFill = false;
    for (size_t k = rank + 1; k < n; ++k) {
        std::string word;
        if (text(argIn[k], word)) {
            data = k;
            method = word;
            if (k + 1 < n) {
                if (k + 2 != n || !hostValues(argIn[k + 1], fill) || fill.size() != 1) {
                    return false;
                }
                useFill = true;
            }
            break;
        }
    }
    if (method != "linear" && method != "nearest") {
        return false;
    }
    bool explicitGrid = data == 2 * rank + 1;
    if (!explicitGrid && data != rank + 1) {
        return false;
    }
    size_t vIndex = explicitGrid ? rank : 0;
    std::vector<ArrayOf> owners(2 * rank + 1);
    GpuArrayObject* v = GpuEngineGateway::gpuArrayDeviceOperand(argIn[vIndex], owners[0], true);
    if (v == nullptr) {
        return false;
    }
    std::vector<GpuArrayObject*> grids(rank, nullptr);
    std::vector<GpuArrayObject*> queries(rank, nullptr);
    for (size_t axis = 0; axis < rank; ++axis) {
        // argument position of this array dimension
        size_t position = axis;
        if (meshgridOrder && axis < 2) {
            position = 1 - axis;
        }
        queries[axis] = GpuEngineGateway::gpuArrayDeviceOperand(
            argIn[vIndex + 1 + position], owners[1 + axis], true);
        if (explicitGrid) {
            grids[axis] = GpuEngineGateway::gpuArrayDeviceOperand(
                argIn[position], owners[1 + rank + axis], true);
        } else {
            indexType count = v->dimensions().dimensionLength((indexType)axis);
            single* steps = (single*)ArrayOf::allocateArrayOf(NLS_SINGLE, count);
            for (indexType k = 0; k < count; ++k) {
                steps[k] = (single)(k + 1);
            }
            owners[1 + rank + axis]
                = CreateGpuArray(ArrayOf(NLS_SINGLE, Dimensions(1, count), steps));
            grids[axis] = AsGpuArrayObject(owners[1 + rank + axis]);
        }
        if (queries[axis] == nullptr || grids[axis] == nullptr) {
            return false;
        }
    }
    return GpuInterpGrid(
        grids, v, queries, method == "nearest", useFill, useFill ? fill[0] : 0.0f, result);
}
//=============================================================================
ArrayOfVector
Nelson::GpuEngineGateway::gpuArrayInterp2Builtin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOf result;
    if (nLhs <= 1 && argIn.size() >= 3 && gridInterpolation(argIn, 2, true, result)) {
        return one(result);
    }
    return host(eval, L"interp2", nLhs, argIn);
}
//=============================================================================
ArrayOfVector
Nelson::GpuEngineGateway::gpuArrayInterp3Builtin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOf result;
    if (nLhs <= 1 && argIn.size() >= 4 && gridInterpolation(argIn, 3, true, result)) {
        return one(result);
    }
    return host(eval, L"interp3", nLhs, argIn);
}
//=============================================================================
ArrayOfVector
Nelson::GpuEngineGateway::gpuArrayInterpnBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOf result;
    if (nLhs <= 1 && argIn.size() >= 3) {
        // the number of dimensions comes from the argument count
        size_t n = argIn.size();
        size_t data = n;
        std::string word;
        for (size_t k = 1; k < n; ++k) {
            if (text(argIn[k], word)) {
                data = k;
                break;
            }
        }
        for (size_t rank : { (size_t)2, (size_t)3 }) {
            if ((data == 2 * rank + 1 || data == rank + 1)
                && gridInterpolation(argIn, rank, false, result)) {
                return one(result);
            }
        }
    }
    return host(eval, L"interpn", nLhs, argIn);
}
//=============================================================================
ArrayOfVector
Nelson::GpuEngineGateway::gpuArrayFilterBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    // filter(b, a, x) and filter(b, a, x, [], dim); initial conditions stay on the host
    std::vector<float> b, a;
    int dim = -1;
    bool shapeOk = argIn.size() == 3
        || (argIn.size() == 5 && argIn[3].isEmpty() && !AsGpuArrayObject(argIn[3])
            && scalarInt(argIn[4], dim));
    if (shapeOk && nLhs <= 1 && hostValues(argIn[0], b) && hostValues(argIn[1], a)) {
        ArrayOf ownerX, result;
        GpuArrayObject* x = GpuEngineGateway::gpuArrayDeviceOperand(argIn[2], ownerX, true);
        if (x && GpuFilter(b, a, x, dim, result)) {
            return one(result);
        }
    }
    return host(eval, L"filter", nLhs, argIn);
}
//=============================================================================
ArrayOfVector
Nelson::GpuEngineGateway::gpuArrayInterp1Builtin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    // (x, v, xq), (x, v, xq, method), (x, v, xq, method, 'extrap' | value), (v, xq [, method])
    ArrayOfVector args(argIn);
    size_t n = args.size();
    std::string method = "linear";
    bool implicitX = n == 2 || (n == 3 && text(args[2], method));
    ArrayOf ownerX, ownerV, ownerQ, result;
    if (nLhs > 1 || n < 2) {
        return host(eval, L"interp1", nLhs, argIn);
    }
    if (implicitX) {
        ArrayOf vValue = args[0];
        GpuArrayObject* vg = GpuEngineGateway::gpuArrayDeviceOperand(vValue, ownerV, true);
        if (vg == nullptr) {
            return host(eval, L"interp1", nLhs, argIn);
        }
        indexType count = vg->elementCount();
        single* steps = (single*)ArrayOf::allocateArrayOf(NLS_SINGLE, count);
        for (indexType k = 0; k < count; ++k) {
            steps[k] = (single)(k + 1);
        }
        ArrayOf xs(NLS_SINGLE, Dimensions(1, count), steps);
        args = ArrayOfVector();
        args << xs << vValue << argIn[1];
        if (n == 3) {
            args << argIn[2];
        }
        n = args.size();
    }
    int extrapMode = 0;
    float fill = 0.0f;
    if (n >= 4 && !text(args[3], method)) {
        return host(eval, L"interp1", nLhs, argIn);
    }
    if (n == 5) {
        std::string option;
        std::vector<float> value;
        if (text(args[4], option) && option == "extrap") {
            extrapMode = 2;
        } else if (hostValues(args[4], value) && value.size() == 1) {
            extrapMode = 1;
            fill = value[0];
        } else {
            return host(eval, L"interp1", nLhs, argIn);
        }
    }
    if (n > 5) {
        return host(eval, L"interp1", nLhs, argIn);
    }
    GpuArrayObject* x = GpuEngineGateway::gpuArrayDeviceOperand(args[0], ownerX, true);
    GpuArrayObject* v = GpuEngineGateway::gpuArrayDeviceOperand(args[1], ownerV, true);
    GpuArrayObject* q = GpuEngineGateway::gpuArrayDeviceOperand(args[2], ownerQ, true);
    if (x && v && q && GpuInterp1(x, v, q, method, extrapMode, fill, result)) {
        return one(result);
    }
    return host(eval, L"interp1", nLhs, argIn);
}
//=============================================================================
ArrayOfVector
Nelson::GpuEngineGateway::gpuArrayTrapzBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    return trapzLike(eval, nLhs, argIn, false);
}
//=============================================================================
ArrayOfVector
Nelson::GpuEngineGateway::gpuArrayCumtrapzBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    return trapzLike(eval, nLhs, argIn, true);
}
//=============================================================================
