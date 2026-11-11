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
#include <functional>
#include "gpuReductionsBuiltin.hpp"
#include "gpuOperatorsBuiltin.hpp"
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
// dimension and flag arguments shared by the reductions
struct ReduceOptions
{
    bool all = false;
    std::vector<int> dims;
    bool omitNan = false;
    bool includeNan = false;
    std::string outType;
};
//=============================================================================
using DimKernel = std::function<ArrayOf(GpuArrayObject*, int)>;
//=============================================================================
// false when an argument is not understood here (the host then runs the call)
bool
parseOptions(const ArrayOfVector& argIn, size_t first, ReduceOptions& options)
{
    for (size_t k = first; k < argIn.size(); ++k) {
        const ArrayOf& arg = argIn[k];
        if (arg.isRowVectorCharacterArray() || arg.isScalarStringArray()) {
            std::string flag = arg.asUtf8String();
            std::transform(flag.begin(), flag.end(), flag.begin(), ::tolower);
            if (flag == "all") {
                options.all = true;
            } else if (flag == "omitnan" || flag == "omitmissing") {
                options.omitNan = true;
            } else if (flag == "includenan" || flag == "includemissing") {
                options.includeNan = true;
            } else if (flag == "double" || flag == "native" || flag == "default") {
                options.outType = flag;
            } else {
                return false;
            }
            continue;
        }
        GpuArrayObject* gpuDim = AsGpuArrayObject(arg);
        ArrayOf dimArg = gpuDim ? GatherGpuArray(gpuDim) : arg;
        if (!dimArg.isNumeric() || dimArg.isEmpty() || !options.dims.empty()) {
            return false;
        }
        dimArg.promoteType(NLS_DOUBLE);
        const double* values = (const double*)dimArg.dataPointer();
        for (indexType i = 0; i < dimArg.elementCount(); ++i) {
            if (values[i] < 1 || values[i] != (double)(int)values[i]) {
                return false;
            }
            options.dims.push_back((int)values[i]);
        }
        std::sort(options.dims.begin(), options.dims.end());
        if (std::unique(options.dims.begin(), options.dims.end()) != options.dims.end()) {
            return false;
        }
    }
    return true;
}
//=============================================================================
// a 0-by-0 input without a dimension reduces to one value (max and min keep it empty)
bool
isEmptySquare(const ArrayOf& input, const ReduceOptions& options)
{
    GpuArrayObject* a = AsGpuArrayObject(input);
    Dimensions dims = a->dimensions();
    return !options.all && options.dims.empty() && dims.length() == 2 && dims.rows() == 0
        && dims.columns() == 0;
}
//=============================================================================
ArrayOf
emptyResult(float value, const std::string& type)
{
    return GpuFilled(Dimensions(1, 1), value, type);
}
//=============================================================================
// a vector of dimensions reduces one dimension after the other
ArrayOf
reduceOver(const ArrayOf& input, const ReduceOptions& options, const DimKernel& kernel)
{
    GpuArrayObject* a = AsGpuArrayObject(input);
    if (options.all) {
        // column view of the same data: no copy
        ArrayOf column = ArrayOf::handle(GpuArrayObject::view(a, Dimensions(a->elementCount(), 1)));
        return kernel(AsGpuArrayObject(column), 1);
    }
    if (options.dims.empty()) {
        return kernel(a, -1);
    }
    ArrayOf current = input;
    for (int dim : options.dims) {
        current = kernel(AsGpuArrayObject(current), dim);
    }
    return current;
}
//=============================================================================
bool
isFloatValues(GpuArrayObject* a)
{
    return !a->isRawIntStorage() && GpuIntegerResultType(a, a).empty() && !a->isLogical();
}
//=============================================================================
// NaN replaced by the identity of the reduction
ArrayOf
withoutNan(GpuArrayObject* a, const char* identity)
{
    return GpuUnaryElementwise(a, std::string("gpuArray:omitnan:") + identity,
        "select(ai, " + std::string(identity) + ", " + GpuWgslIsNan("ai") + ")");
}
//=============================================================================
ArrayOfVector
hostCall(Evaluator* eval, const wchar_t* name, int nLhs, const ArrayOfVector& argIn)
{
    return GpuEngineGateway::gpuArrayHostFallback(eval, name, nLhs, argIn);
}
//=============================================================================
GpuArrayObject*
gpuInput(const ArrayOf& value)
{
    GpuArrayObject* a = AsGpuArrayObject(value);
    if (a == nullptr) {
        Error(L"Nelson:gpuArray:notGpuArray", L"gpuArray expected.");
    }
    return a;
}
//=============================================================================
// sum and prod: X, dim | vecdim | 'all', outtype, nanflag
ArrayOfVector
sumProd(Evaluator* eval, int nLhs, const ArrayOfVector& argIn, bool isSum)
{
    nargincheck(argIn, 1);
    nargoutcheck(nLhs, 0, 1);
    const wchar_t* name = isSum ? L"sum" : L"prod";
    GpuArrayObject* a = gpuInput(argIn[0]);
    ReduceOptions options;
    // a native integer sum saturates step by step, which only the in-order host loop reproduces
    if (!parseOptions(argIn, 1, options) || (options.includeNan && options.omitNan)
        || (options.outType == "native" && !isFloatValues(a))) {
        return hostCall(eval, name, nLhs, argIn);
    }
    if (isEmptySquare(argIn[0], options)) {
        ArrayOfVector empty;
        empty << emptyResult(isSum ? 0.0f : 1.0f, "single");
        return empty;
    }
    ArrayOf input = argIn[0];
    if (options.omitNan && isFloatValues(a)) {
        input = withoutNan(a, isSum ? "0.0" : "1.0");
    }
    DimKernel kernel = isSum ? DimKernel(GpuSum) : DimKernel(GpuProd);
    ArrayOf result = reduceOver(input, options, kernel);
    ArrayOfVector retval;
    retval << result;
    return retval;
}
//=============================================================================
// any and all: X, dim | vecdim | 'all'
ArrayOfVector
anyAll(Evaluator* eval, int nLhs, const ArrayOfVector& argIn, bool isAny)
{
    nargincheck(argIn, 1, 2);
    nargoutcheck(nLhs, 0, 1);
    gpuInput(argIn[0]);
    ReduceOptions options;
    if (!parseOptions(argIn, 1, options) || options.omitNan || options.includeNan
        || !options.outType.empty()) {
        return hostCall(eval, isAny ? L"any" : L"all", nLhs, argIn);
    }
    ArrayOfVector retval;
    if (isEmptySquare(argIn[0], options)) {
        retval << emptyResult(isAny ? 0.0f : 1.0f, "logical");
        return retval;
    }
    retval << reduceOver(argIn[0], options, isAny ? DimKernel(GpuAny) : DimKernel(GpuAll));
    return retval;
}
//=============================================================================
} // namespace
//=============================================================================
ArrayOfVector
Nelson::GpuEngineGateway::gpuArraySumBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    return sumProd(eval, nLhs, argIn, true);
}
//=============================================================================
ArrayOfVector
Nelson::GpuEngineGateway::gpuArrayProdBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    return sumProd(eval, nLhs, argIn, false);
}
//=============================================================================
ArrayOfVector
Nelson::GpuEngineGateway::gpuArrayAnyBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    return anyAll(eval, nLhs, argIn, true);
}
//=============================================================================
ArrayOfVector
Nelson::GpuEngineGateway::gpuArrayAllBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    return anyAll(eval, nLhs, argIn, false);
}
//=============================================================================
// mean: X, dim | vecdim | 'all', outtype, nanflag
ArrayOfVector
Nelson::GpuEngineGateway::gpuArrayMeanBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1);
    nargoutcheck(nLhs, 0, 1);
    GpuArrayObject* a = gpuInput(argIn[0]);
    ReduceOptions options;
    if (!parseOptions(argIn, 1, options) || (options.includeNan && options.omitNan)
        || (options.outType == "native" && a->isLogical())) {
        return hostCall(eval, L"mean", nLhs, argIn);
    }
    ArrayOf result;
    if (isEmptySquare(argIn[0], options)) {
        result = emptyResult(std::nanf(""), "single");
    } else if (options.omitNan && isFloatValues(a)) {
        // sum of the numbers over the count of numbers
        ArrayOf sum = reduceOver(withoutNan(a, "0.0"), options, GpuSum);
        ArrayOf numbers = GpuUnaryElementwise(
            a, "gpuArray:isnumber", "f32(!" + GpuWgslIsNan("ai") + ")", "single");
        ArrayOf count = reduceOver(numbers, options, GpuSum);
        result = GpuRdivide(AsGpuArrayObject(sum), AsGpuArrayObject(count));
    } else {
        result = reduceOver(argIn[0], options, GpuMean);
    }
    if (options.outType == "native" && a->underlyingType() != "single") {
        result = GpuCast(result, a->underlyingType());
    }
    ArrayOfVector retval;
    retval << result;
    return retval;
}
//=============================================================================
#define GPU_SCAN_BUILTIN(BUILTIN_NAME, KERNEL_FN, NAME)                                            \
    ArrayOfVector Nelson::GpuEngineGateway::BUILTIN_NAME(                                          \
        Evaluator* eval, int nLhs, const ArrayOfVector& argIn)                                     \
    {                                                                                              \
        nargincheck(argIn, 1);                                                                     \
        nargoutcheck(nLhs, 0, 1);                                                                  \
        GpuArrayObject* a = gpuInput(argIn[0]);                                                    \
        bool scalarDim = argIn.size() == 2 && !AsGpuArrayObject(argIn[1]) && argIn[1].isNumeric()  \
            && argIn[1].isScalar();                                                                \
        if (argIn.size() > 2 || (argIn.size() == 2 && !scalarDim)) {                               \
            return hostCall(eval, NAME, nLhs, argIn);                                              \
        }                                                                                          \
        int dim = (argIn.size() == 2) ? (int)argIn[1].asScalarIndex(false) : -1;                   \
        ArrayOfVector retval;                                                                      \
        retval << KERNEL_FN(a, dim);                                                               \
        return retval;                                                                             \
    }
GPU_SCAN_BUILTIN(gpuArrayCumsumBuiltin, GpuCumsum, L"cumsum")
GPU_SCAN_BUILTIN(gpuArrayCumprodBuiltin, GpuCumprod, L"cumprod")
#undef GPU_SCAN_BUILTIN
//=============================================================================
// var/std: X, w, dim | 'all'. w == 1 normalizes by N, otherwise by N-1.
static ArrayOfVector
varStd(Evaluator* eval, int nLhs, const ArrayOfVector& argIn, bool isStd)
{
    nargincheck(argIn, 1);
    nargoutcheck(nLhs, 0, 1);
    gpuInput(argIn[0]);
    const wchar_t* name = isStd ? L"std" : L"var";
    bool normalizeByN = false;
    if (argIn.size() >= 2) {
        if (AsGpuArrayObject(argIn[1]) || !argIn[1].isNumeric()
            || !(argIn[1].isEmpty() || argIn[1].isScalar())) {
            return hostCall(eval, name, nLhs, argIn);
        }
        normalizeByN = !argIn[1].isEmpty() && argIn[1].asDoubleScalar() == 1.0;
    }
    ReduceOptions options;
    if (!parseOptions(argIn, 2, options) || options.omitNan || !options.outType.empty()
        || options.dims.size() > 1) {
        return hostCall(eval, name, nLhs, argIn);
    }
    auto kernel = [isStd, normalizeByN](GpuArrayObject* a, int dim) {
        return isStd ? GpuStd(a, dim, normalizeByN) : GpuVar(a, dim, normalizeByN);
    };
    ArrayOfVector retval;
    retval << reduceOver(argIn[0], options, kernel);
    return retval;
}
//=============================================================================
ArrayOfVector
Nelson::GpuEngineGateway::gpuArrayVarBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    return varStd(eval, nLhs, argIn, false);
}
//=============================================================================
ArrayOfVector
Nelson::GpuEngineGateway::gpuArrayStdBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    return varStd(eval, nLhs, argIn, true);
}
//=============================================================================
// max/min: X | X, Y [, nanflag] | X, [], dim | vecdim | 'all' [, nanflag]
static ArrayOfVector
maxMin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn, bool isMax)
{
    nargincheck(argIn, 1);
    nargoutcheck(nLhs, 0, 2);
    const wchar_t* name = isMax ? L"max" : L"min";
    bool wantIndices = (nLhs >= 2);
    // two arrays compared element by element
    if (argIn.size() >= 2 && !(argIn[1].isEmpty() && !AsGpuArrayObject(argIn[1]))) {
        ReduceOptions flags;
        if (wantIndices || !parseOptions(argIn, 2, flags) || !flags.dims.empty() || flags.all
            || !flags.outType.empty()) {
            return hostCall(eval, name, nLhs, argIn);
        }
        GpuArrayObject* a = AsGpuArrayObject(argIn[0]);
        GpuArrayObject* b = AsGpuArrayObject(argIn[1]);
        ArrayOf ownedA = a ? argIn[0] : CreateGpuArray(argIn[0]);
        ArrayOf ownedB = b ? argIn[1] : CreateGpuArray(argIn[1]);
        ArrayOfVector retval;
        retval << (isMax
                ? GpuMaxBinary(AsGpuArrayObject(ownedA), AsGpuArrayObject(ownedB), flags.includeNan)
                : GpuMinBinary(
                      AsGpuArrayObject(ownedA), AsGpuArrayObject(ownedB), flags.includeNan));
        return retval;
    }
    gpuInput(argIn[0]);
    ReduceOptions options;
    if (!parseOptions(argIn, 2, options) || !options.outType.empty()
        || (wantIndices && options.dims.size() > 1)) {
        return hostCall(eval, name, nLhs, argIn);
    }
    bool includeNan = options.includeNan;
    if (wantIndices) {
        ArrayOfVector indexed;
        auto kernel = [&indexed, isMax, includeNan](GpuArrayObject* a, int dim) {
            indexed = isMax ? GpuMaxAlong2(a, dim, includeNan) : GpuMinAlong2(a, dim, includeNan);
            return indexed[0];
        };
        reduceOver(argIn[0], options, kernel);
        return indexed;
    }
    auto kernel = [isMax, includeNan](GpuArrayObject* a, int dim) {
        return isMax ? GpuMaxAlong(a, dim, includeNan) : GpuMinAlong(a, dim, includeNan);
    };
    ArrayOfVector retval;
    retval << reduceOver(argIn[0], options, kernel);
    return retval;
}
//=============================================================================
ArrayOfVector
Nelson::GpuEngineGateway::gpuArrayMaxBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    return maxMin(eval, nLhs, argIn, true);
}
//=============================================================================
ArrayOfVector
Nelson::GpuEngineGateway::gpuArrayMinBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    return maxMin(eval, nLhs, argIn, false);
}
//=============================================================================
// p from a numeric scalar or 'fro' / 'inf' / '-inf'; false when it is something else
static bool
normOrder(const ArrayOf& arg, double& p, bool& frobenius)
{
    frobenius = false;
    if (arg.isRowVectorCharacterArray() || arg.isScalarStringArray()) {
        std::string s = arg.asUtf8String();
        std::transform(s.begin(), s.end(), s.begin(), ::tolower);
        if (s == "fro") {
            frobenius = true;
            p = 2.0;
            return true;
        }
        if (s == "inf" || s == "-inf") {
            p = (s == "inf") ? INFINITY : -INFINITY;
            return true;
        }
        return false;
    }
    if (AsGpuArrayObject(arg) || !arg.isNumeric() || !arg.isScalar() || arg.isComplex()) {
        return false;
    }
    p = arg.asDoubleScalar();
    return !std::isnan(p) && p != 0.0;
}
//=============================================================================
ArrayOfVector
Nelson::GpuEngineGateway::gpuArrayNormBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    GpuArrayObject* a = AsGpuArrayObject(argIn.empty() ? ArrayOf() : argIn[0]);
    double p = 2.0;
    bool frobenius = false;
    if (a == nullptr || argIn.size() > 2 || nLhs > 1
        || (argIn.size() == 2 && !normOrder(argIn[1], p, frobenius)) || a->isRawIntStorage()
        || !GpuIntegerResultType(a, a).empty()) {
        return hostCall(eval, L"norm", nLhs, argIn);
    }
    Dimensions dims = a->dimensions();
    bool vector = dims.length() == 2 && (dims.rows() <= 1 || dims.columns() <= 1);
    ArrayOfVector retval;
    if (a->elementCount() == 0) {
        retval << GpuFilled(Dimensions(1, 1), 0.0f, "single");
        return retval;
    }
    if (vector || frobenius) {
        // every element of a vector (or of any array for 'fro') in one lane, no copy
        ArrayOf column = ArrayOf::handle(GpuArrayObject::view(a, Dimensions(a->elementCount(), 1)));
        retval << GpuVecnorm(AsGpuArrayObject(column), p, 1);
        return retval;
    }
    if (dims.length() == 2 && (p == 1.0 || (std::isinf(p) && p > 0))) {
        // largest column sum (1) or row sum (Inf) of the magnitudes
        ArrayOf sums = GpuVecnorm(a, 1.0, p == 1.0 ? 1 : 2);
        GpuArrayObject* s = AsGpuArrayObject(sums);
        ArrayOf column = ArrayOf::handle(GpuArrayObject::view(s, Dimensions(s->elementCount(), 1)));
        retval << GpuMaxAlong(AsGpuArrayObject(column), 1, true);
        return retval;
    }
    return hostCall(eval, L"norm", nLhs, argIn);
}
//=============================================================================
ArrayOfVector
Nelson::GpuEngineGateway::gpuArrayVecnormBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    GpuArrayObject* a = AsGpuArrayObject(argIn.empty() ? ArrayOf() : argIn[0]);
    double p = 2.0;
    bool frobenius = false;
    int dim = -1;
    bool ok = a != nullptr && argIn.size() <= 3 && nLhs <= 1 && !a->isRawIntStorage()
        && GpuIntegerResultType(a, a).empty();
    if (ok && argIn.size() >= 2) {
        ok = normOrder(argIn[1], p, frobenius) && !frobenius && p > 0;
    }
    if (ok && argIn.size() == 3) {
        ReduceOptions options;
        ok = parseOptions(argIn, 2, options) && options.dims.size() == 1 && !options.all;
        dim = ok ? options.dims[0] : -1;
    }
    if (!ok) {
        return hostCall(eval, L"vecnorm", nLhs, argIn);
    }
    ArrayOfVector retval;
    retval << GpuVecnorm(a, p, dim);
    return retval;
}
//=============================================================================
// median: X, dim | vecdim | 'all', nanflag
ArrayOfVector
Nelson::GpuEngineGateway::gpuArrayMedianBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    GpuArrayObject* a = AsGpuArrayObject(argIn.empty() ? ArrayOf() : argIn[0]);
    ReduceOptions options;
    if (a == nullptr || nLhs > 1 || !parseOptions(argIn, 1, options) || !options.outType.empty()
        || options.dims.size() > 1 || !isFloatValues(a) || a->isComplex()) {
        return hostCall(eval, L"median", nLhs, argIn);
    }
    ArrayOfVector retval;
    if (isEmptySquare(argIn[0], options)) {
        retval << emptyResult(std::nanf(""), "single");
        return retval;
    }
    bool omitNan = options.omitNan;
    retval << reduceOver(argIn[0], options,
        [omitNan](GpuArrayObject* g, int dim) { return GpuMedian(g, dim, omitNan); });
    return retval;
}
//=============================================================================
// rms: X, dim | 'all'; the root of the mean square computed as norm / sqrt(n)
ArrayOfVector
Nelson::GpuEngineGateway::gpuArrayRmsBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    GpuArrayObject* a = AsGpuArrayObject(argIn.empty() ? ArrayOf() : argIn[0]);
    ReduceOptions options;
    if (a == nullptr || nLhs > 1 || !parseOptions(argIn, 1, options) || !options.outType.empty()
        || options.omitNan || options.dims.size() > 1 || !isFloatValues(a)
        || a->elementCount() == 0) {
        return hostCall(eval, L"rms", nLhs, argIn);
    }
    ArrayOfVector retval;
    retval << reduceOver(argIn[0], options, [](GpuArrayObject* g, int dim) {
        // length of the reduced dimension (first one not of length 1 by default)
        Dimensions dims = g->dimensions();
        indexType ndims = std::max<indexType>(dims.length(), 2);
        int used = dim;
        if (used <= 0) {
            used = (int)ndims;
            for (indexType k = 0; k < ndims; ++k) {
                if (dims.dimensionLength(k) != 1) {
                    used = (int)k + 1;
                    break;
                }
            }
        }
        indexType length = (used <= (int)ndims) ? dims.dimensionLength(used - 1) : 1;
        ArrayOf norms = GpuVecnorm(g, 2.0, dim);
        if (length <= 1) {
            return norms;
        }
        ArrayOf scale = GpuFilled(Dimensions(1, 1), 1.0f / std::sqrt((float)length), "single");
        return GpuTimes(AsGpuArrayObject(norms), AsGpuArrayObject(scale));
    });
    return retval;
}
//=============================================================================
// bounds: [smallest, largest] = bounds(X, dim | vecdim | 'all', nanflag)
ArrayOfVector
Nelson::GpuEngineGateway::gpuArrayBoundsBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    GpuArrayObject* a = AsGpuArrayObject(argIn.empty() ? ArrayOf() : argIn[0]);
    ReduceOptions options;
    if (a == nullptr || !parseOptions(argIn, 1, options) || !options.outType.empty()) {
        return hostCall(eval, L"bounds", nLhs, argIn);
    }
    bool includeNan = options.includeNan;
    ArrayOfVector retval;
    retval << reduceOver(argIn[0], options,
        [includeNan](GpuArrayObject* g, int dim) { return GpuMinAlong(g, dim, includeNan); });
    if (nLhs > 1) {
        retval << reduceOver(argIn[0], options,
            [includeNan](GpuArrayObject* g, int dim) { return GpuMaxAlong(g, dim, includeNan); });
    }
    return retval;
}
//=============================================================================
