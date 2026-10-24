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
#include <complex>
#include "VarianceOptions.hpp"
#include "Variance.hpp"
#include "Mean.hpp"
#include "Permute.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static bool
tryGetDimensionVector(const ArrayOf& arg, std::vector<indexType>& dims);
static ArrayOf
allElementsAsColumn(const ArrayOf& x, const VarianceOptions& options);
static indexType
operatingDimension(const Dimensions& dims, int dim);
static ArrayOf
meanAlong(const ArrayOf& x, indexType workDim, bool omitNaN, bool& needToOverload);
static ArrayOf
meanOverDimensions(
    const ArrayOf& x, const std::vector<indexType>& dims, bool omitNaN, bool& needToOverload);
static ArrayOf
weightedMean(const ArrayOf& x, const ArrayOf& weights, int dim, bool omitNaN, bool& needToOverload);
//=============================================================================
VarianceOptions
ParseVarianceOptions(const ArrayOfVector& argIn)
{
    VarianceOptions options;
    size_t nbNumericArgs = argIn.size();
    // trailing character arguments: 'omitnan' | 'includenan' | 'all'
    while (nbNumericArgs > 1) {
        ArrayOf lastArg = argIn[nbNumericArgs - 1];
        if (!lastArg.isRowVectorCharacterArray()
            && !(lastArg.isStringArray() && lastArg.isScalar())) {
            break;
        }
        std::wstring flag = lastArg.asWideString();
        if (flag == L"omitnan" || flag == L"omitmissing") {
            options.omitNaN = true;
        } else if (flag == L"includenan" || flag == L"includemissing") {
            options.omitNaN = false;
        } else if (flag == L"all") {
            options.allElements = true;
        } else {
            Error(L"Nelson:statistics:invalidNanFlag", L"%s expected.",
                L"Wrong value for argument: 'omitnan' or 'includenan'");
        }
        nbNumericArgs--;
    }
    if (nbNumericArgs > 1) {
        ArrayOf arg2 = argIn[1];
        if (arg2.isEmpty(false)) {
            options.weight = 0;
        } else if (arg2.isScalar()) {
            options.weight = (int)arg2.asScalarIndex(true);
            if (options.weight != 0 && options.weight != 1) {
                Error(L"Nelson:statistics:invalidValue:wrongValueNArgument",
                    L"Wrong value for #%d argument.", 2);
            }
        } else {
            if (!arg2.isNumeric() || arg2.isComplex() || !arg2.isVector()) {
                Error(L"Nelson:statistics:invalidWeight",
                    L"The weight must be a real vector or a scalar 0 or 1.");
            }
            options.weightVector = arg2;
            options.hasWeightVector = true;
        }
    }
    if (nbNumericArgs > 2) {
        ArrayOf arg3 = argIn[2];
        if (tryGetDimensionVector(arg3, options.vecDims)) {
            options.useVecDim = true;
        } else {
            options.dim = (int)arg3.asScalarIndex(true);
            if (options.dim <= 0) {
                Error(L"Nelson:statistics:invalidValue:wrongValueNArgument",
                    L"Wrong value for #%d argument.", 3);
            }
        }
    }
    if (options.useVecDim && options.hasWeightVector) {
        Error(L"Nelson:statistics:weightWithDimVector",
            L"A weight vector is not supported with a vector of dimensions.");
    }
    return options;
}
//=============================================================================
ArrayOf
PrepareVarianceInput(const ArrayOf& x)
{
    ArrayOf data(x);
    if (data.isSparse()) {
        data.makeDense();
    }
    NelsonType nlsType = data.dataClass();
    if (nlsType == NLS_LOGICAL || nlsType == NLS_CHAR || IS_INTEGER_TYPE(nlsType)) {
        data.promoteType(NLS_DOUBLE);
    }
    return data;
}
//=============================================================================
ArrayOf
VarianceWithOptions(const ArrayOf& x, const VarianceOptions& options, bool& needToOverload)
{
    ArrayOf data = allElementsAsColumn(x, options);
    if (options.useVecDim) {
        return Variance(data, options.weight, options.vecDims, options.omitNaN, needToOverload);
    }
    int dim = options.allElements ? -1 : options.dim;
    if (options.hasWeightVector) {
        return Variance(data, options.weightVector, dim, options.omitNaN, needToOverload);
    }
    return Variance(data, options.weight, dim, options.omitNaN, needToOverload);
}
//=============================================================================
ArrayOf
VarianceMean(const ArrayOf& x, const VarianceOptions& options, bool& needToOverload)
{
    needToOverload = false;
    if (x.isEmpty(true) && options.dim < 0 && !options.useVecDim) {
        // Same convention as the variance of a 0-by-0 array.
        bool isSingle = (x.dataClass() == NLS_SINGLE || x.dataClass() == NLS_SCOMPLEX);
        return isSingle ? ArrayOf::singleScalar((single)std::nan(""))
                        : ArrayOf::doubleScalar(std::nan(""));
    }
    ArrayOf data = allElementsAsColumn(x, options);
    if (options.useVecDim) {
        return meanOverDimensions(data, options.vecDims, options.omitNaN, needToOverload);
    }
    int dim = options.allElements ? -1 : options.dim;
    if (options.hasWeightVector) {
        return weightedMean(data, options.weightVector, dim, options.omitNaN, needToOverload);
    }
    return meanAlong(
        data, operatingDimension(data.dimensions(), dim), options.omitNaN, needToOverload);
}
//=============================================================================
bool
CollapseOperatingDimensions(const ArrayOf& A, const std::vector<indexType>& dims,
    ArrayOf& collapsed, Dimensions& outDim, int& singleDim, bool& needToOverload)
{
    needToOverload = false;
    Dimensions dimsA = A.dimensions();
    indexType nd = dimsA.length();
    // Keep only the dimensions that actually exist: any listed dimension beyond
    // ndims is a singleton whose reduction is a no-op. Duplicates collapse too.
    std::vector<bool> isReduced(nd, false);
    for (indexType d : dims) {
        if (d >= 1 && d <= nd) {
            isReduced[d - 1] = true;
        }
    }
    std::vector<indexType> reduced;
    std::vector<indexType> kept;
    for (indexType k = 0; k < nd; ++k) {
        if (isReduced[k]) {
            reduced.push_back(k + 1);
        } else {
            kept.push_back(k + 1);
        }
    }
    if (reduced.size() <= 1) {
        // A single effective operating dimension (also covers a vector that
        // only lists trailing singleton dimensions).
        singleDim = reduced.empty() ? (dims.empty() ? 0 : (int)dims.front()) : (int)reduced.front();
        return false;
    }
    // Move the reduced dimensions to the front and collapse them into one:
    // reducing over several dimensions spans all their elements at once.
    std::vector<indexType> perm(reduced);
    perm.insert(perm.end(), kept.begin(), kept.end());
    collapsed = Permute(A, perm, needToOverload);
    if (needToOverload) {
        return false;
    }
    indexType reducedCount = 1;
    for (indexType d : reduced) {
        reducedCount *= dimsA[d - 1];
    }
    indexType keptCount = 1;
    for (indexType d : kept) {
        keptCount *= dimsA[d - 1];
    }
    Dimensions collapsedDims(reducedCount, keptCount);
    collapsed.reshape(collapsedDims);
    outDim = dimsA;
    for (indexType d : reduced) {
        outDim[d - 1] = 1;
    }
    outDim.simplify();
    return true;
}
//=============================================================================
static bool
tryGetDimensionVector(const ArrayOf& arg, std::vector<indexType>& dims)
{
    // A non-scalar numeric vector of positive integers (e.g. var(A, w, [1 2])).
    if (arg.isRowVectorCharacterArray() || arg.isStringArray() || arg.isScalar() || !arg.isNumeric()
        || arg.isComplex() || !arg.isVector()) {
        return false;
    }
    ArrayOf values(arg);
    values.promoteType(NLS_DOUBLE);
    const double* ptr = (const double*)values.dataPointer();
    dims.clear();
    for (indexType k = 0; k < values.elementCount(); ++k) {
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
static ArrayOf
allElementsAsColumn(const ArrayOf& x, const VarianceOptions& options)
{
    ArrayOf data(x);
    if (options.allElements && !data.isScalar()) {
        Dimensions columnDims(data.elementCount(), 1);
        data.reshape(columnDims);
    }
    return data;
}
//=============================================================================
static indexType
operatingDimension(const Dimensions& dims, int dim)
{
    if (dim > 0) {
        return (indexType)(dim - 1);
    }
    indexType d = 0;
    while (d < dims.length() && dims.dimensionLength(d) == 1) {
        d++;
    }
    return (d == dims.length()) ? 0 : d;
}
//=============================================================================
static ArrayOf
meanAlong(const ArrayOf& x, indexType workDim, bool omitNaN, bool& needToOverload)
{
    if (x.dimensions().dimensionLength(workDim) != 0) {
        return Mean(x, workDim + 1, omitNaN, MEAN_OUT_TYPE::DEFAULT, needToOverload);
    }
    // No observation along the operating dimension: the mean of each slice is
    // NaN, like the variance.
    needToOverload = false;
    Dimensions outDims(x.dimensions());
    outDims.setDimensionLength(workDim, 1);
    bool isSingle = (x.dataClass() == NLS_SINGLE || x.dataClass() == NLS_SCOMPLEX);
    NelsonType outClass = isSingle ? NLS_SINGLE : NLS_DOUBLE;
    indexType n = outDims.elementCount();
    void* ptr = ArrayOf::allocateArrayOf(outClass, n);
    for (indexType k = 0; k < n; ++k) {
        if (isSingle) {
            ((single*)ptr)[k] = std::nanf("");
        } else {
            ((double*)ptr)[k] = std::nan("");
        }
    }
    return ArrayOf(outClass, outDims, ptr);
}
//=============================================================================
static ArrayOf
meanOverDimensions(
    const ArrayOf& x, const std::vector<indexType>& dims, bool omitNaN, bool& needToOverload)
{
    ArrayOf collapsed;
    Dimensions outDim;
    int singleDim = 0;
    if (!CollapseOperatingDimensions(x, dims, collapsed, outDim, singleDim, needToOverload)) {
        if (needToOverload) {
            return {};
        }
        return meanAlong(x, operatingDimension(x.dimensions(), singleDim), omitNaN, needToOverload);
    }
    ArrayOf res = meanAlong(collapsed, 0, omitNaN, needToOverload);
    if (!needToOverload) {
        res.reshape(outDim);
    }
    return res;
}
//=============================================================================
template <class T>
static bool
isNaNValue(const T& value)
{
    return std::isnan(value);
}
//=============================================================================
template <class T>
static bool
isNaNValue(const std::complex<T>& value)
{
    return std::isnan(value.real()) || std::isnan(value.imag());
}
//=============================================================================
// Weighted mean along one dimension: sum(w .* x) / sum(w), NaN elements (and
// their weights) skipped when omitNaN is set. Acc is the accumulator type.
template <class T, class Acc>
static ArrayOf
weightedMeanLines(
    const ArrayOf& x, NelsonType outClass, const double* weight, indexType dim, bool omitNaN)
{
    Dimensions dims = x.dimensions();
    indexType lineSize = dims[dim];
    Dimensions outDims(dims);
    outDims.setDimensionLength(dim, 1);
    outDims.simplify();
    indexType planeSize = 1;
    for (indexType k = 0; k < dim; k++) {
        planeSize *= dims[k];
    }
    indexType planes = 1;
    for (indexType k = dim + 1; k < dims.length(); k++) {
        planes *= dims[k];
    }
    const T* src = (const T*)x.dataPointer();
    T* dst = (T*)ArrayOf::allocateArrayOf(outClass, outDims.elementCount());
    for (indexType i = 0; i < planes; i++) {
        for (indexType j = 0; j < planeSize; j++) {
            double sumWeights = 0;
            Acc sumValues = Acc(0);
            const T* line = src + i * planeSize * lineSize + j;
            for (indexType k = 0; k < lineSize; k++) {
                const T& value = line[k * planeSize];
                if (omitNaN && isNaNValue(value)) {
                    continue;
                }
                sumWeights += weight[k];
                sumValues += Acc(value) * weight[k];
            }
            dst[i * planeSize + j] = (T)(sumValues / sumWeights);
        }
    }
    return ArrayOf(outClass, outDims, dst);
}
//=============================================================================
static ArrayOf
weightedMean(const ArrayOf& x, const ArrayOf& weights, int dim, bool omitNaN, bool& needToOverload)
{
    needToOverload = false;
    indexType workDim = operatingDimension(x.dimensions(), dim);
    ArrayOf w(weights);
    w.promoteType(NLS_DOUBLE);
    if (w.elementCount() != x.dimensions().dimensionLength(workDim)) {
        Error(
            L"Nelson:statistics:weightSizeMismatch", L"The length of W must be compatible with X.");
    }
    const double* wgt = (const double*)w.dataPointer();
    switch (x.dataClass()) {
    case NLS_SINGLE:
        return weightedMeanLines<single, double>(x, NLS_SINGLE, wgt, workDim, omitNaN);
    case NLS_DOUBLE:
        return weightedMeanLines<double, double>(x, NLS_DOUBLE, wgt, workDim, omitNaN);
    case NLS_SCOMPLEX:
        return weightedMeanLines<std::complex<single>, std::complex<double>>(
            x, NLS_SCOMPLEX, wgt, workDim, omitNaN);
    case NLS_DCOMPLEX:
        return weightedMeanLines<std::complex<double>, std::complex<double>>(
            x, NLS_DCOMPLEX, wgt, workDim, omitNaN);
    default:
        needToOverload = true;
        return {};
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
