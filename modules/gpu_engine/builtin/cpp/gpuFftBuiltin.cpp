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
#include "gpuFftBuiltin.hpp"
#include "gpuFallbackBuiltin.hpp"
#include "GpuArrayObject.hpp"
#include "GpuFft.hpp"
#include "GpuKernels.hpp"
#include "GpuTransfer.hpp"
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
// real host scalar >= 0 (an empty matrix means the default)
bool
lengthArg(const ArrayOf& value, indexType& out, bool& given)
{
    given = false;
    if (AsGpuArrayObject(value)) {
        return false;
    }
    if (value.isEmpty() && value.isNumeric()) {
        return true;
    }
    if (!value.isNumeric() || !value.isScalar() || value.isComplex()) {
        return false;
    }
    double d = value.asDoubleScalar();
    if (d < 0 || d != (double)(indexType)d) {
        return false;
    }
    out = (indexType)d;
    given = true;
    return true;
}
//=============================================================================
indexType
lengthAlong(GpuArrayObject* a, int dim)
{
    Dimensions d = a->dimensions();
    return (dim - 1 < (int)d.length()) ? d.dimensionLength(dim - 1) : 1;
}
//=============================================================================
int
defaultDim(GpuArrayObject* a)
{
    Dimensions d = a->dimensions();
    for (indexType k = 0; k < d.length(); ++k) {
        if (d.dimensionLength(k) != 1) {
            return (int)k + 1;
        }
    }
    return 1;
}
//=============================================================================
bool
usable(GpuArrayObject* a)
{
    return a != nullptr && !a->isRawIntStorage() && GpuIntegerResultType(a, a).empty();
}
//=============================================================================
// transform along each listed dimension in turn
ArrayOf
transform(ArrayOf current, const std::vector<std::pair<int, indexType>>& steps, bool inverse)
{
    for (const auto& step : steps) {
        current = GpuFftAlong(AsGpuArrayObject(current), step.second, step.first, inverse);
    }
    return current;
}
//=============================================================================
// an inverse transform is real when its input is conjugate symmetric (or 'symmetric' is set)
ArrayOf
realWhenSymmetric(const ArrayOf& input, const ArrayOf& output, const std::vector<int>& dims,
    bool sizesKept, int symmetryFlag)
{
    if (symmetryFlag == 2) {
        return output;
    }
    bool real = symmetryFlag == 1;
    if (!real && sizesKept) {
        GpuArrayObject* g = AsGpuArrayObject(input);
        if (g->isComplex()) {
            real = GpuIsConjugateSymmetric(g, dims);
        } else {
            GpuArrayObject* promoted = GpuPromoteToComplex(g);
            ArrayOf owner = ArrayOf::handle(promoted);
            real = GpuIsConjugateSymmetric(promoted, dims);
        }
    }
    return real ? GpuReal(AsGpuArrayObject(output)) : output;
}
//=============================================================================
// trailing 'symmetric' (1) or 'nonsymmetric' (2) of the inverse transforms; 0 when absent
bool
symmetryOption(ArrayOfVector& args, int& flag)
{
    flag = 0;
    if (args.empty()) {
        return true;
    }
    const ArrayOf& last = args.back();
    if (!(last.isRowVectorCharacterArray() || last.isScalarStringArray())) {
        return true;
    }
    std::string option = last.asUtf8String();
    if (option == "symmetric") {
        flag = 1;
    } else if (option == "nonsymmetric") {
        flag = 2;
    } else {
        return false;
    }
    args.pop_back();
    return true;
}
//=============================================================================
ArrayOfVector
oneDimensional(Evaluator* eval, int nLhs, const ArrayOfVector& argIn, bool inverse)
{
    const wchar_t* name = inverse ? L"ifft" : L"fft";
    ArrayOfVector args(argIn);
    int flag = 0;
    if (inverse && !symmetryOption(args, flag)) {
        return host(eval, name, nLhs, argIn);
    }
    GpuArrayObject* a = AsGpuArrayObject(args.empty() ? ArrayOf() : args[0]);
    if (!usable(a) || args.size() > 3 || nLhs > 1) {
        return host(eval, name, nLhs, argIn);
    }
    int dim = defaultDim(a);
    if (args.size() == 3) {
        indexType d = 0;
        bool given = false;
        if (!lengthArg(args[2], d, given) || !given || d < 1) {
            return host(eval, name, nLhs, argIn);
        }
        dim = (int)d;
    }
    indexType n = lengthAlong(a, dim);
    if (args.size() >= 2) {
        indexType value = 0;
        bool given = false;
        if (!lengthArg(args[1], value, given)) {
            return host(eval, name, nLhs, argIn);
        }
        if (given) {
            n = value;
        }
    }
    bool kept = n == lengthAlong(a, dim);
    ArrayOf out = GpuFftAlong(a, n, dim, inverse);
    ArrayOfVector retval;
    if (n == 1 && !a->isComplex()) {
        // a single-point transform of real data stays real
        retval << GpuReal(AsGpuArrayObject(out));
        return retval;
    }
    if (!inverse && !a->isComplex()) {
        GpuMakeConjugateSymmetric(AsGpuArrayObject(out), { dim });
    }
    retval << (inverse ? realWhenSymmetric(args[0], out, { dim }, kept, flag) : out);
    return retval;
}
//=============================================================================
ArrayOfVector
twoDimensional(Evaluator* eval, int nLhs, const ArrayOfVector& argIn, bool inverse)
{
    const wchar_t* name = inverse ? L"ifft2" : L"fft2";
    ArrayOfVector args(argIn);
    int flag = 0;
    if (inverse && !symmetryOption(args, flag)) {
        return host(eval, name, nLhs, argIn);
    }
    GpuArrayObject* a = AsGpuArrayObject(args.empty() ? ArrayOf() : args[0]);
    if (!usable(a) || (args.size() != 1 && args.size() != 3) || nLhs > 1) {
        return host(eval, name, nLhs, argIn);
    }
    indexType m = lengthAlong(a, 1);
    indexType n = lengthAlong(a, 2);
    if (args.size() == 3) {
        bool g1 = false, g2 = false;
        indexType m2 = m, n2 = n;
        if (!lengthArg(args[1], m2, g1) || !lengthArg(args[2], n2, g2) || !g1 || !g2) {
            return host(eval, name, nLhs, argIn);
        }
        m = m2;
        n = n2;
    }
    bool kept = m == lengthAlong(a, 1) && n == lengthAlong(a, 2);
    ArrayOf out = transform(args[0], { { 1, m }, { 2, n } }, inverse);
    if (!inverse && !a->isComplex()) {
        GpuMakeConjugateSymmetric(AsGpuArrayObject(out), { 1, 2 });
    }
    ArrayOfVector retval;
    retval << (inverse ? realWhenSymmetric(args[0], out, { 1, 2 }, kept, flag) : out);
    return retval;
}
//=============================================================================
ArrayOfVector
allDimensions(Evaluator* eval, int nLhs, const ArrayOfVector& argIn, bool inverse)
{
    const wchar_t* name = inverse ? L"ifftn" : L"fftn";
    ArrayOfVector args(argIn);
    int flag = 0;
    if (inverse && !symmetryOption(args, flag)) {
        return host(eval, name, nLhs, argIn);
    }
    GpuArrayObject* a = AsGpuArrayObject(args.empty() ? ArrayOf() : args[0]);
    if (!usable(a) || args.size() != 1 || nLhs > 1) {
        return host(eval, name, nLhs, argIn);
    }
    std::vector<std::pair<int, indexType>> steps;
    std::vector<int> dims;
    Dimensions d = a->dimensions();
    for (indexType k = 0; k < d.length(); ++k) {
        steps.push_back({ (int)k + 1, d.dimensionLength(k) });
        dims.push_back((int)k + 1);
    }
    ArrayOf out = transform(args[0], steps, inverse);
    if (!inverse && !a->isComplex()) {
        GpuMakeConjugateSymmetric(AsGpuArrayObject(out), dims);
    }
    ArrayOfVector retval;
    retval << (inverse ? realWhenSymmetric(args[0], out, dims, true, flag) : out);
    return retval;
}
//=============================================================================
} // namespace
//=============================================================================
ArrayOfVector
Nelson::GpuEngineGateway::gpuArrayFftBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    return oneDimensional(eval, nLhs, argIn, false);
}
//=============================================================================
ArrayOfVector
Nelson::GpuEngineGateway::gpuArrayIfftBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    return oneDimensional(eval, nLhs, argIn, true);
}
//=============================================================================
ArrayOfVector
Nelson::GpuEngineGateway::gpuArrayFft2Builtin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    return twoDimensional(eval, nLhs, argIn, false);
}
//=============================================================================
ArrayOfVector
Nelson::GpuEngineGateway::gpuArrayIfft2Builtin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    return twoDimensional(eval, nLhs, argIn, true);
}
//=============================================================================
ArrayOfVector
Nelson::GpuEngineGateway::gpuArrayFftnBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    return allDimensions(eval, nLhs, argIn, false);
}
//=============================================================================
ArrayOfVector
Nelson::GpuEngineGateway::gpuArrayIfftnBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    return allDimensions(eval, nLhs, argIn, true);
}
//=============================================================================
