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
#include <vector>
#include "meanBuiltin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "OverloadRequired.hpp"
#include "Mean.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
static bool
tryGetDimensionVector(const ArrayOf& arg, std::vector<indexType>& dims)
{
    if (arg.isRowVectorCharacterArray() || arg.isStringArray() || arg.isScalar() || !arg.isNumeric()
        || arg.isComplex() || !arg.isVector()) {
        return false;
    }
    ArrayOf values(arg);
    values.promoteType(NLS_DOUBLE);
    const double* ptr = (const double*)values.dataPointer();
    indexType n = values.elementCount();
    dims.clear();
    for (indexType k = 0; k < n; ++k) {
        double v = ptr[k];
        if (v < 1 || v != std::floor(v)) {
            Error(L"Nelson:statistics:invalidDimensionArg", L"Dimension argument must be %s.",
                L"a positive integer scalar or vector of positive integers");
        }
        dims.push_back((indexType)v);
    }
    return true;
}
//=============================================================================
// One pass over X: each element is added (as double) to the output slot it falls in.
template <class T>
static void
accumulateOverDimensions(const T* sp, bool isComplex, bool omitNaN,
    const std::vector<indexType>& sizes, const std::vector<indexType>& outStride,
    std::vector<double>& re, std::vector<double>& im, std::vector<double>& kept)
{
    const size_t nd = sizes.size();
    std::vector<indexType> coord(nd, 0);
    indexType total = 1;
    for (indexType s : sizes) {
        total *= s;
    }
    indexType out = 0;
    for (indexType i = 0; i < total; ++i) {
        double vr = isComplex ? (double)sp[2 * i] : (double)sp[i];
        double vi = isComplex ? (double)sp[2 * i + 1] : 0.0;
        if (!omitNaN || !(std::isnan(vr) || std::isnan(vi))) {
            re[out] += vr;
            im[out] += vi;
            kept[out] += 1.0;
        }
        for (size_t d = 0; d < nd; ++d) {
            out += outStride[d];
            if (++coord[d] < sizes[d]) {
                break;
            }
            out -= outStride[d] * sizes[d];
            coord[d] = 0;
        }
    }
}
//=============================================================================
// Mean over a vector of dimensions as sum / count (NaN excluded from both when omitted);
// integers are averaged in double and rounded once for a 'native' output.
static ArrayOf
meanOverDimensions(const ArrayOf& X, const std::vector<indexType>& dims, bool omitNaN,
    MEAN_OUT_TYPE outType, bool& needToOverload)
{
    NelsonType classX = X.dataClass();
    if (classX > NLS_LOGICAL) {
        // other classes keep the single-dimension rules (errors, overloads)
        ArrayOf acc(X);
        for (indexType dm : dims) {
            acc = Mean(acc, dm, omitNaN, outType, needToOverload);
            if (needToOverload) {
                break;
            }
        }
        return acc;
    }
    Dimensions dimsX = X.dimensions();
    const indexType nd = dimsX.length();
    std::vector<bool> reduced(nd, false);
    for (indexType d : dims) {
        if (d >= 1 && d <= nd) {
            reduced[d - 1] = true;
        }
    }
    std::vector<indexType> sizes(nd);
    std::vector<indexType> outStride(nd, 0);
    Dimensions dimsOut = dimsX;
    indexType stride = 1;
    for (indexType d = 0; d < nd; ++d) {
        sizes[d] = dimsX.at(d);
        if (reduced[d]) {
            dimsOut.setDimensionLength(d, 1);
        } else {
            outStride[d] = stride;
            stride *= sizes[d];
        }
    }
    dimsOut.simplify();
    const indexType outCount = dimsOut.elementCount();
    std::vector<double> re(outCount, 0.0);
    std::vector<double> im(outCount, 0.0);
    std::vector<double> kept(outCount, 0.0);
    const bool isComplex = X.isComplex();
    const void* sp = X.dataPointer();
#define NLS_MEAN_DIMS_CASE(NLSTYPE, CTYPE)                                                         \
    case NLSTYPE:                                                                                  \
        accumulateOverDimensions(                                                                  \
            (const CTYPE*)sp, isComplex, omitNaN, sizes, outStride, re, im, kept);                 \
        break;
    switch (classX) {
        NLS_MEAN_DIMS_CASE(NLS_DOUBLE, double)
        NLS_MEAN_DIMS_CASE(NLS_DCOMPLEX, double)
        NLS_MEAN_DIMS_CASE(NLS_SINGLE, single)
        NLS_MEAN_DIMS_CASE(NLS_SCOMPLEX, single)
        NLS_MEAN_DIMS_CASE(NLS_INT8, int8)
        NLS_MEAN_DIMS_CASE(NLS_UINT8, uint8)
        NLS_MEAN_DIMS_CASE(NLS_INT16, int16)
        NLS_MEAN_DIMS_CASE(NLS_UINT16, uint16)
        NLS_MEAN_DIMS_CASE(NLS_INT32, int32)
        NLS_MEAN_DIMS_CASE(NLS_UINT32, uint32)
        NLS_MEAN_DIMS_CASE(NLS_INT64, int64)
        NLS_MEAN_DIMS_CASE(NLS_UINT64, uint64)
        NLS_MEAN_DIMS_CASE(NLS_LOGICAL, logical)
    default:
        break;
    }
#undef NLS_MEAN_DIMS_CASE
    double* pr = static_cast<double*>(ArrayOf::allocateArrayOf(
        isComplex ? NLS_DCOMPLEX : NLS_DOUBLE, outCount, stringVector(), false));
    for (indexType k = 0; k < outCount; ++k) {
        if (isComplex) {
            // an empty slice gives NaN with a zero imaginary part
            pr[2 * k] = re[k] / kept[k];
            pr[2 * k + 1] = kept[k] == 0.0 ? 0.0 : im[k] / kept[k];
        } else {
            pr[k] = re[k] / kept[k];
        }
    }
    ArrayOf res(isComplex ? NLS_DCOMPLEX : NLS_DOUBLE, dimsOut, pr);
    bool singleInput = classX == NLS_SINGLE || classX == NLS_SCOMPLEX;
    if (outType != MEAN_OUT_TYPE::DOUBLE && singleInput) {
        res.promoteType(isComplex ? NLS_SCOMPLEX : NLS_SINGLE);
    } else if (outType == MEAN_OUT_TYPE::NATIVE && X.isIntegerType()) {
        res.promoteType(classX);
    }
    return res;
}
//=============================================================================
// A logical 'native' mean divides the saturated logical sum by the count: any(X) / n.
static ArrayOf
logicalNativeMean(const ArrayOf& X, const ArrayOf& trueMean)
{
    const indexType outCount = trueMean.elementCount();
    if (X.isEmpty(false) || outCount == 0) {
        return trueMean;
    }
    const double invCount = 1.0 / (double)(X.elementCount() / outCount);
    const double* src = static_cast<const double*>(trueMean.dataPointer());
    double* dst = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, outCount));
    for (indexType k = 0; k < outCount; ++k) {
        dst[k] = src[k] > 0.0 ? invCount : src[k];
    }
    return ArrayOf(NLS_DOUBLE, trueMean.dimensions(), dst);
}
//=============================================================================
static bool
isTextArgument(const ArrayOf& arg)
{
    return arg.isRowVectorCharacterArray() || (arg.isStringArray() && arg.isScalar());
}
//=============================================================================
static const char*
argumentOrdinal(size_t k)
{
    switch (k) {
    case 1:
        return "second";
    case 2:
        return "third";
    default:
        return "4th";
    }
}
//=============================================================================
ArrayOfVector
Nelson::StatisticsGateway::meanBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 4);
    nargoutcheck(nLhs, 0, 1);
    if (argIn[0].isSparse()) {
        // a sparse input is averaged densely; the mean stays sparse
        ArrayOfVector denseArgs(argIn);
        denseArgs[0].makeDense();
        retval = meanBuiltin(nLhs, denseArgs);
        if (!retval.empty()) {
            retval[0].makeSparse();
        }
        return retval;
    }
    for (size_t k = 1; k < argIn.size(); ++k) {
        // 'omitmissing' and 'includemissing' are the 'omitnan' and 'includenan' flags
        if (isTextArgument(argIn[k])) {
            std::wstring flag = argIn[k].asWideString();
            if (flag == L"omitmissing" || flag == L"includemissing") {
                ArrayOfVector renamed(argIn);
                renamed[k]
                    = ArrayOf::characterArray(flag == L"omitmissing" ? L"omitnan" : L"includenan");
                return meanBuiltin(nLhs, renamed);
            }
        }
    }
    bool needToOverload = false;
    bool omitNaN = false;
    bool isAll = false;
    bool useVecDim = false;
    std::vector<indexType> vecDims;
    indexType dim = 0;
    MEAN_OUT_TYPE outType = MEAN_OUT_TYPE::DEFAULT;
    // mean(A, dim | vecdim | 'all', flags...): flags follow in any order, each kind once
    bool haveNaNChoice = false;
    bool haveTypeChoice = false;
    for (size_t k = 1; k < argIn.size(); ++k) {
        const ArrayOf& arg = argIn[k];
        if (!isTextArgument(arg)) {
            if (k != 1) {
                Error("Nelson:statistics:invalidValue:invalidArgument", "Invalid %s argument.",
                    argumentOrdinal(k));
            }
            if (tryGetDimensionVector(arg, vecDims)) {
                useVecDim = true;
            } else {
                dim = arg.asScalarIndex(false);
            }
            continue;
        }
        std::wstring s = arg.asWideString();
        if (s == L"all" && k == 1) {
            isAll = true;
        } else if ((s == L"omitnan" || s == L"includenan") && !haveNaNChoice) {
            omitNaN = s == L"omitnan";
            haveNaNChoice = true;
        } else if ((s == L"default" || s == L"double" || s == L"native") && !haveTypeChoice) {
            outType = s == L"native" ? MEAN_OUT_TYPE::NATIVE
                : s == L"double"     ? MEAN_OUT_TYPE::DOUBLE
                                     : MEAN_OUT_TYPE::DEFAULT;
            haveTypeChoice = true;
        } else {
            Error("Nelson:statistics:invalidValue:invalidArgument", "Invalid %s argument.",
                argumentOrdinal(k));
        }
    }
    ArrayOf res;
    if (isAll) {
        res = MeanAll(argIn[0], omitNaN, outType, needToOverload);
    } else if (useVecDim) {
        res = meanOverDimensions(argIn[0], vecDims, omitNaN, outType, needToOverload);
    } else {
        res = Mean(argIn[0], dim, omitNaN, outType, needToOverload);
    }
    if (needToOverload) {
        OverloadRequired("mean");
    } else {
        if (outType == MEAN_OUT_TYPE::NATIVE && argIn[0].isLogical()) {
            res = logicalNativeMean(argIn[0], res);
        }
        retval << res;
    }
    return retval;
}
//=============================================================================
