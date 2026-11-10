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
#include "gpuRearrangeBuiltin.hpp"
#include "gpuFallbackBuiltin.hpp"
#include "GpuArrayObject.hpp"
#include "GpuKernels.hpp"
#include "GpuTransfer.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
namespace {
//=============================================================================
// whole numbers of a real host or device argument; false when it is something else
bool
integerValues(const ArrayOf& arg, std::vector<int64_t>& values, bool allowNegative)
{
    GpuArrayObject* gpuArg = AsGpuArrayObject(arg);
    ArrayOf host = gpuArg ? GatherGpuArray(gpuArg) : arg;
    if (!(host.isNumeric() || host.isLogical()) || host.isSparse() || host.isComplex()) {
        return false;
    }
    host.promoteType(NLS_DOUBLE);
    const double* p = (const double*)host.dataPointer();
    values.clear();
    for (indexType k = 0; k < host.elementCount(); ++k) {
        if (!std::isfinite(p[k]) || p[k] != std::floor(p[k]) || (!allowNegative && p[k] < 0)) {
            return false;
        }
        values.push_back((int64_t)p[k]);
    }
    return true;
}
//=============================================================================
std::vector<indexType>
asIndices(const std::vector<int64_t>& values)
{
    return std::vector<indexType>(values.begin(), values.end());
}
//=============================================================================
ArrayOfVector
host(Evaluator* eval, const wchar_t* name, int nLhs, const ArrayOfVector& argIn)
{
    return GpuEngineGateway::gpuArrayHostFallback(eval, name, nLhs, argIn);
}
//=============================================================================
ArrayOfVector
oneResult(const ArrayOf& value)
{
    ArrayOfVector retval;
    retval << value;
    return retval;
}
//=============================================================================
// device copy of a host vector argument (kept alive by `owner`)
GpuArrayObject*
onDevice(const ArrayOf& value, ArrayOf& owner)
{
    GpuArrayObject* gpuObject = AsGpuArrayObject(value);
    if (gpuObject != nullptr) {
        return gpuObject;
    }
    owner = CreateGpuArray(value);
    return AsGpuArrayObject(owner);
}
//=============================================================================
bool
isPermutation(const std::vector<int64_t>& order, size_t ndims)
{
    if (order.size() < ndims) {
        return false;
    }
    std::vector<int64_t> sorted(order);
    std::sort(sorted.begin(), sorted.end());
    for (size_t k = 0; k < sorted.size(); ++k) {
        if (sorted[k] != (int64_t)k + 1) {
            return false;
        }
    }
    return true;
}
//=============================================================================
// grids where output k follows input k along dimension `axisOf(k)`
ArrayOfVector
grids(Evaluator* eval, const wchar_t* name, int nLhs, const ArrayOfVector& argIn, bool meshgrid)
{
    // one input stands for the same vector in every direction (meshgrid builds at least 2)
    size_t count = std::max<size_t>(argIn.size(), (size_t)std::max(nLhs, 1));
    if (meshgrid && argIn.size() == 1) {
        count = std::max<size_t>(count, 2);
    }
    if (argIn.empty() || (meshgrid && count > 3) || (argIn.size() != 1 && argIn.size() < count)) {
        return host(eval, name, nLhs, argIn);
    }
    std::vector<ArrayOf> owners(count);
    std::vector<GpuArrayObject*> vectors;
    for (size_t k = 0; k < count; ++k) {
        const ArrayOf& value = argIn[std::min(k, argIn.size() - 1)];
        GpuArrayObject* gpuObject = AsGpuArrayObject(value);
        bool isVector = gpuObject ? gpuObject->dimensions().isVector() || gpuObject->dimensions().isScalar()
                                  : (value.isVector() || value.isScalar());
        if (!isVector || (!gpuObject && !(value.isNumeric() || value.isLogical()))) {
            return host(eval, name, nLhs, argIn);
        }
        vectors.push_back(onDevice(value, owners[k]));
    }
    // meshgrid puts the first input along columns and the second along rows
    std::vector<indexType> lengths;
    for (GpuArrayObject* v : vectors) {
        lengths.push_back(v->elementCount());
    }
    if (meshgrid && count >= 2) {
        std::swap(lengths[0], lengths[1]);
    }
    if (lengths.size() == 1) {
        lengths.push_back(1);
    }
    ArrayOfVector retval;
    for (size_t k = 0; k < std::max<size_t>(nLhs, 1) && k < count; ++k) {
        size_t axis = k;
        if (meshgrid && count >= 2 && k < 2) {
            axis = 1 - k;
        }
        retval << GpuGridAxis(vectors[k], lengths, axis);
    }
    return retval;
}
//=============================================================================
} // namespace
//=============================================================================
ArrayOfVector
Nelson::GpuEngineGateway::gpuArrayPermuteBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    std::vector<int64_t> order;
    GpuArrayObject* a = AsGpuArrayObject(argIn.empty() ? ArrayOf() : argIn[0]);
    if (argIn.size() != 2 || a == nullptr || !integerValues(argIn[1], order, false)
        || !isPermutation(order, a->dimensions().length()) || nLhs > 1) {
        return host(eval, L"permute", nLhs, argIn);
    }
    return oneResult(GpuPermute(a, asIndices(order)));
}
//=============================================================================
ArrayOfVector
Nelson::GpuEngineGateway::gpuArrayIpermuteBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    std::vector<int64_t> order;
    GpuArrayObject* a = AsGpuArrayObject(argIn.empty() ? ArrayOf() : argIn[0]);
    if (argIn.size() != 2 || a == nullptr || !integerValues(argIn[1], order, false)
        || !isPermutation(order, a->dimensions().length()) || nLhs > 1) {
        return host(eval, L"ipermute", nLhs, argIn);
    }
    std::vector<indexType> inverse(order.size());
    for (size_t k = 0; k < order.size(); ++k) {
        inverse[(size_t)order[k] - 1] = k + 1;
    }
    return oneResult(GpuPermute(a, inverse));
}
//=============================================================================
ArrayOfVector
Nelson::GpuEngineGateway::gpuArrayShiftdimBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    GpuArrayObject* a = AsGpuArrayObject(argIn.empty() ? ArrayOf() : argIn[0]);
    if (a == nullptr || argIn.size() > 2) {
        return host(eval, L"shiftdim", nLhs, argIn);
    }
    Dimensions dims = a->dimensions();
    size_t ndims = dims.length();
    if (argIn.size() == 1) {
        // drop the leading singleton dimensions and report how many
        size_t lead = 0;
        while (lead + 1 < ndims && dims.dimensionLength(lead) == 1) {
            lead++;
        }
        if (dims.isScalar()) {
            lead = 0;
        }
        std::vector<indexType> rest;
        for (size_t k = lead; k < ndims; ++k) {
            rest.push_back(dims.dimensionLength(k));
        }
        while (rest.size() < 2) {
            rest.push_back(1);
        }
        ArrayOfVector retval;
        retval << GpuReshape(a, Dimensions(rest));
        if (nLhs > 1) {
            retval << ArrayOf::doubleScalar((double)lead);
        }
        return retval;
    }
    std::vector<int64_t> n;
    if (!integerValues(argIn[1], n, true) || n.size() != 1 || nLhs > 1) {
        return host(eval, L"shiftdim", nLhs, argIn);
    }
    if (n[0] < 0) {
        std::vector<indexType> grown((size_t)-n[0], 1);
        for (size_t k = 0; k < ndims; ++k) {
            grown.push_back(dims.dimensionLength(k));
        }
        return oneResult(GpuReshape(a, Dimensions(grown)));
    }
    size_t shift = (size_t)n[0] % std::max<size_t>(ndims, 1);
    std::vector<indexType> order;
    for (size_t k = 0; k < ndims; ++k) {
        order.push_back((k + shift) % ndims + 1);
    }
    return oneResult(GpuPermute(a, order));
}
//=============================================================================
ArrayOfVector
Nelson::GpuEngineGateway::gpuArrayRepmatBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    GpuArrayObject* a = AsGpuArrayObject(argIn.empty() ? ArrayOf() : argIn[0]);
    std::vector<int64_t> tiles;
    bool valid = a != nullptr && argIn.size() >= 2 && nLhs <= 1;
    for (size_t k = 1; valid && k < argIn.size(); ++k) {
        std::vector<int64_t> part;
        valid = integerValues(argIn[k], part, true)
            && (argIn.size() == 2 || part.size() == 1);
        tiles.insert(tiles.end(), part.begin(), part.end());
    }
    if (!valid || tiles.empty()) {
        return host(eval, L"repmat", nLhs, argIn);
    }
    if (tiles.size() == 1) {
        tiles.push_back(tiles[0]);
    }
    for (int64_t& t : tiles) {
        t = std::max<int64_t>(t, 0);
    }
    return oneResult(GpuRepeat(a, {}, asIndices(tiles)));
}
//=============================================================================
ArrayOfVector
Nelson::GpuEngineGateway::gpuArrayRepelemBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    GpuArrayObject* a = AsGpuArrayObject(argIn.empty() ? ArrayOf() : argIn[0]);
    if (a == nullptr || argIn.size() < 2 || nLhs > 1) {
        return host(eval, L"repelem", nLhs, argIn);
    }
    std::vector<int64_t> copies;
    if (argIn.size() == 2) {
        // a vector repeats each element along its own direction
        std::vector<int64_t> n;
        if (!a->dimensions().isVector() && !a->dimensions().isScalar()) {
            return host(eval, L"repelem", nLhs, argIn);
        }
        if (!integerValues(argIn[1], n, false) || n.size() != 1) {
            return host(eval, L"repelem", nLhs, argIn);
        }
        bool row = a->dimensions().rows() == 1;
        copies = row ? std::vector<int64_t> { 1, n[0] } : std::vector<int64_t> { n[0], 1 };
    } else {
        for (size_t k = 1; k < argIn.size(); ++k) {
            std::vector<int64_t> n;
            if (!integerValues(argIn[k], n, false) || n.size() != 1) {
                return host(eval, L"repelem", nLhs, argIn);
            }
            copies.push_back(n[0]);
        }
    }
    return oneResult(GpuRepeat(a, asIndices(copies), {}));
}
//=============================================================================
ArrayOfVector
Nelson::GpuEngineGateway::gpuArrayCircshiftBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    GpuArrayObject* a = AsGpuArrayObject(argIn.empty() ? ArrayOf() : argIn[0]);
    std::vector<int64_t> k;
    if (a == nullptr || argIn.size() < 2 || argIn.size() > 3 || nLhs > 1
        || !integerValues(argIn[1], k, true) || k.empty()) {
        return host(eval, L"circshift", nLhs, argIn);
    }
    Dimensions dims = a->dimensions();
    std::vector<int64_t> shifts;
    if (argIn.size() == 3) {
        std::vector<int64_t> dim;
        if (!integerValues(argIn[2], dim, false) || dim.size() != 1 || dim[0] < 1 || k.size() != 1) {
            return host(eval, L"circshift", nLhs, argIn);
        }
        shifts.assign((size_t)dim[0], 0);
        shifts[(size_t)dim[0] - 1] = k[0];
    } else if (k.size() == 1) {
        // a scalar shift moves the first dimension longer than 1
        size_t first = 0;
        while (first < dims.length() && dims.dimensionLength(first) == 1) {
            first++;
        }
        if (first == dims.length()) {
            first = 0;
        }
        shifts.assign(first + 1, 0);
        shifts[first] = k[0];
    } else {
        shifts = k;
    }
    return oneResult(GpuCircshiftND(a, shifts));
}
//=============================================================================
ArrayOfVector
Nelson::GpuEngineGateway::gpuArrayMeshgridBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    return grids(eval, L"meshgrid", nLhs, argIn, true);
}
//=============================================================================
ArrayOfVector
Nelson::GpuEngineGateway::gpuArrayNdgridBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    return grids(eval, L"ndgrid", nLhs, argIn, false);
}
//=============================================================================
// half-length circular shifts that move the zero frequency to the centre and back
static ArrayOfVector
halfShift(Evaluator* eval, const wchar_t* name, int nLhs, const ArrayOfVector& argIn, bool inverse)
{
    GpuArrayObject* a = AsGpuArrayObject(argIn.empty() ? ArrayOf() : argIn[0]);
    std::vector<int64_t> dim;
    if (a == nullptr || argIn.size() > 2 || nLhs > 1
        || (argIn.size() == 2 && (!integerValues(argIn[1], dim, false) || dim.size() != 1 || dim[0] < 1))) {
        return host(eval, name, nLhs, argIn);
    }
    Dimensions dims = a->dimensions();
    std::vector<int64_t> shifts(argIn.size() == 2 ? (size_t)dim[0] : dims.length(), 0);
    for (size_t k = 0; k < shifts.size(); ++k) {
        if (argIn.size() == 2 && k + 1 != (size_t)dim[0]) {
            continue;
        }
        int64_t n = (k < dims.length()) ? (int64_t)dims.dimensionLength(k) : 1;
        shifts[k] = inverse ? -(n / 2) : n / 2;
    }
    return oneResult(GpuCircshiftND(a, shifts));
}
//=============================================================================
ArrayOfVector
Nelson::GpuEngineGateway::gpuArrayFftshiftBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    return halfShift(eval, L"fftshift", nLhs, argIn, false);
}
//=============================================================================
ArrayOfVector
Nelson::GpuEngineGateway::gpuArrayIfftshiftBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    return halfShift(eval, L"ifftshift", nLhs, argIn, true);
}
//=============================================================================
