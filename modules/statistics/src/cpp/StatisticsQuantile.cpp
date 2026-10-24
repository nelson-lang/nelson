//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "StatisticsQuantile.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include <algorithm>
#include <cmath>
#include <limits>
#include <numeric>
#include <vector>
//=============================================================================
namespace Nelson {
//=============================================================================
namespace {
    //=============================================================================
    enum class QuantileMethod
    {
        Midpoint,
        Inclusive,
        Exclusive
    };
    //=============================================================================
    struct QuantilePlan
    {
        bool all { false };
        std::vector<indexType> opDims;
        QuantileMethod method { QuantileMethod::Midpoint };
    };
    //=============================================================================
    struct QuantileResult
    {
        ArrayOf values;
        std::vector<double> raw;
        Dimensions baseDims;
        Dimensions valueDims;
        indexType pCount { 0 };
        indexType groupCount { 0 };
    };
    //=============================================================================
    bool
    isTextScalar(const ArrayOf& value)
    {
        return value.isRowVectorCharacterArray() || value.isScalarStringArray();
    }
    //=============================================================================
    std::wstring
    lowerText(const ArrayOf& value)
    {
        std::wstring text = value.asWideString();
        std::transform(
            text.begin(), text.end(), text.begin(), [](wchar_t c) { return towlower(c); });
        return text;
    }
    //=============================================================================
    bool
    isMethodName(const ArrayOf& value)
    {
        return isTextScalar(value) && lowerText(value) == L"method";
    }
    //=============================================================================
    QuantileMethod
    methodValue(const ArrayOf& value)
    {
        if (!isTextScalar(value)) {
            Error("Nelson:statistics:methodMustBeText",
                "Method must be a string scalar or character vector.");
        }
        std::wstring method = lowerText(value);
        if (method == L"midpoint" || method == L"exact" || method == L"approximate") {
            return QuantileMethod::Midpoint;
        }
        if (method == L"inclusive") {
            return QuantileMethod::Inclusive;
        }
        if (method == L"exclusive") {
            return QuantileMethod::Exclusive;
        }
        Error("Nelson:statistics:invalidQuantileMethod", "Invalid quantile method.");
        return QuantileMethod::Midpoint;
    }
    //=============================================================================
    std::vector<indexType>
    dimensionVector(const ArrayOf& value)
    {
        if (!value.isNumeric() || value.isSparse() || !value.isReal() || !value.isVector()
            || value.isEmpty()) {
            Error("Nelson:statistics:invalidDimensionArg", "Dimension argument must be %s.",
                "a positive integer scalar, vector, or 'all'");
        }
        ArrayOf copy = value;
        copy.promoteType(NLS_DOUBLE);
        const double* ptr = static_cast<const double*>(copy.dataPointer());
        std::vector<indexType> dims;
        dims.reserve(copy.elementCount());
        for (indexType k = 0; k < copy.elementCount(); ++k) {
            double dim = ptr[k];
            if (!std::isfinite(dim) || dim < 1 || std::floor(dim) != dim) {
                Error("Nelson:statistics:invalidDimensionArg", "Dimension argument must be %s.",
                    "a positive integer scalar, vector, or 'all'");
            }
            indexType zeroBased = static_cast<indexType>(dim) - 1;
            if (std::find(dims.begin(), dims.end(), zeroBased) == dims.end()) {
                dims.push_back(zeroBased);
            }
        }
        return dims;
    }
    //=============================================================================
    QuantilePlan
    parsePlan(const ArrayOfVector& args, indexType start)
    {
        QuantilePlan plan;
        for (indexType k = start; k < args.size();) {
            const ArrayOf& arg = args[k];
            if (isMethodName(arg)) {
                if (k + 1 >= args.size()) {
                    Error(
                        "Nelson:statistics:methodRequiresValue", "Method option requires a value.");
                }
                plan.method = methodValue(args[k + 1]);
                k += 2;
            } else if (isTextScalar(arg)) {
                if (lowerText(arg) != L"all") {
                    Error("Nelson:statistics:invalidQuantileOption", "Invalid quantile option.");
                }
                plan.all = true;
                k++;
            } else if (arg.isNumeric()) {
                plan.opDims = dimensionVector(arg);
                k++;
            } else {
                Error("Nelson:statistics:invalidQuantileOption", "Invalid quantile option.");
            }
        }
        return plan;
    }
    //=============================================================================
    std::vector<double>
    probabilities(const ArrayOf& value, bool percent)
    {
        if (!value.isNumeric() || value.isSparse() || !value.isReal() || !value.isVector()
            || value.isEmpty()) {
            if (percent) {
                Error("Nelson:statistics:percentageOutOfRange",
                    "Percentages must be in the range [0,100].");
            }
            Error("Nelson:statistics:probabilityOutOfRange", "%s must be in the range [0,1].",
                "Probabilities");
        }
        ArrayOf copy = value;
        copy.promoteType(NLS_DOUBLE);
        const double* ptr = static_cast<const double*>(copy.dataPointer());
        std::vector<double> p;
        p.reserve(copy.elementCount());
        double scale = percent ? 100.0 : 1.0;
        for (indexType k = 0; k < copy.elementCount(); ++k) {
            double v = ptr[k];
            if (!std::isfinite(v) || v < 0 || v > scale) {
                if (percent) {
                    Error("Nelson:statistics:percentageOutOfRange",
                        "Percentages must be in the range [0,100].");
                }
                Error("Nelson:statistics:probabilityOutOfRange", "%s must be in the range [0,1].",
                    "Probabilities");
            }
            p.push_back(v / scale);
        }
        return p;
    }
    //=============================================================================
    std::vector<double>
    quantileProbabilities(const ArrayOf& value)
    {
        if (value.isScalar()) {
            double v = value.asDoubleScalar();
            if (std::isfinite(v) && v > 1 && std::floor(v) == v) {
                std::vector<double> p(static_cast<indexType>(v));
                for (indexType k = 0; k < p.size(); ++k) {
                    p[k] = static_cast<double>(k + 1) / static_cast<double>(p.size() + 1);
                }
                return p;
            }
        }
        return probabilities(value, false);
    }
    //=============================================================================
    std::vector<indexType>
    inputDims(const Dimensions& dims, const QuantilePlan& plan)
    {
        indexType length = std::max<indexType>(2, dims.length());
        for (indexType dim : plan.opDims) {
            length = std::max(length, dim + 1);
        }
        std::vector<indexType> values(length, 1);
        for (indexType k = 0; k < length; ++k) {
            values[k] = dims.dimensionLength(k);
        }
        return values;
    }
    //=============================================================================
    std::vector<indexType>
    operationDims(const std::vector<indexType>& dims, const QuantilePlan& plan)
    {
        if (plan.all) {
            std::vector<indexType> values(dims.size());
            std::iota(values.begin(), values.end(), 0);
            return values;
        }
        if (!plan.opDims.empty()) {
            return plan.opDims;
        }
        for (indexType k = 0; k < dims.size(); ++k) {
            if (dims[k] != 1) {
                return { k };
            }
        }
        return { 0 };
    }
    //=============================================================================
    std::vector<indexType>
    strides(const std::vector<indexType>& dims)
    {
        std::vector<indexType> values(dims.size(), 1);
        for (indexType k = 1; k < dims.size(); ++k) {
            values[k] = values[k - 1] * dims[k - 1];
        }
        return values;
    }
    //=============================================================================
    std::vector<bool>
    operationMask(indexType count, const std::vector<indexType>& opDims)
    {
        std::vector<bool> mask(count, false);
        for (indexType dim : opDims) {
            if (dim < count) {
                mask[dim] = true;
            }
        }
        return mask;
    }
    //=============================================================================
    Dimensions
    simplifiedDims(const std::vector<indexType>& dims)
    {
        Dimensions result(dims);
        result.simplify();
        return result;
    }
    //=============================================================================
    indexType
    groupIndex(indexType index, const std::vector<indexType>& dims,
        const std::vector<indexType>& inStrides, const std::vector<indexType>& baseStrides,
        const std::vector<bool>& opMask)
    {
        indexType group = 0;
        for (indexType dim = 0; dim < dims.size(); ++dim) {
            indexType coord = (index / inStrides[dim]) % dims[dim];
            if (!opMask[dim]) {
                group += coord * baseStrides[dim];
            }
        }
        return group;
    }
    //=============================================================================
    indexType
    outputIndex(indexType group, const std::vector<indexType>& baseDims,
        const std::vector<indexType>& baseStrides, const std::vector<indexType>& outStrides)
    {
        indexType index = 0;
        for (indexType dim = 0; dim < baseDims.size(); ++dim) {
            indexType coord = (group / baseStrides[dim]) % baseDims[dim];
            index += coord * outStrides[dim];
        }
        return index;
    }
    //=============================================================================
    double
    quantileValue(const std::vector<double>& values, double p, QuantileMethod method)
    {
        indexType n = values.size();
        if (n == 0) {
            return std::numeric_limits<double>::quiet_NaN();
        }
        double h = 1;
        if (method == QuantileMethod::Inclusive) {
            h = 1.0 + static_cast<double>(n - 1) * p;
        } else if (method == QuantileMethod::Exclusive) {
            h = static_cast<double>(n + 1) * p;
        } else {
            h = static_cast<double>(n) * p + 0.5;
        }
        if (h <= 1) {
            return values.front();
        }
        if (h >= n) {
            return values.back();
        }
        double lowerIndex = std::floor(h);
        double fraction = h - lowerIndex;
        indexType k = static_cast<indexType>(lowerIndex) - 1;
        return values[k] + fraction * (values[k + 1] - values[k]);
    }
    //=============================================================================
    QuantileResult
    computeQuantiles(const ArrayOf& x, const std::vector<double>& probs, const QuantilePlan& plan)
    {
        ArrayOf data = x;
        data.promoteType(NLS_DOUBLE);
        const double* xPtr = static_cast<const double*>(data.dataPointer());
        std::vector<indexType> dims = inputDims(data.dimensions(), plan);
        std::vector<indexType> opDims = operationDims(dims, plan);
        std::vector<indexType> baseDims = dims;
        for (indexType dim : opDims) {
            baseDims[dim] = 1;
        }
        indexType qDim = *std::min_element(opDims.begin(), opDims.end());
        std::vector<indexType> valueDims = baseDims;
        valueDims[qDim] = static_cast<indexType>(probs.size());
        std::vector<indexType> inStrides = strides(dims);
        std::vector<indexType> baseStrides = strides(baseDims);
        std::vector<indexType> valueStrides = strides(valueDims);
        std::vector<bool> opMask = operationMask(dims.size(), opDims);
        indexType groupCount = simplifiedDims(baseDims).elementCount();
        indexType pCount = static_cast<indexType>(probs.size());
        std::vector<double> raw(pCount * groupCount, std::numeric_limits<double>::quiet_NaN());
        double* outPtr = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, raw.size()));
        if (groupCount == 1) {
            std::vector<double> values;
            values.reserve(data.elementCount());
            for (indexType k = 0; k < data.elementCount(); ++k) {
                double value = xPtr[k];
                if (!std::isnan(value)) {
                    values.push_back(value);
                }
            }
            std::sort(values.begin(), values.end());
            for (indexType pIndex = 0; pIndex < pCount; ++pIndex) {
                double value = quantileValue(values, probs[pIndex], plan.method);
                raw[pIndex] = value;
                outPtr[pIndex * valueStrides[qDim]] = value;
            }
        } else if (opDims.size() == 1 && opDims[0] == 0) {
            indexType groupLength = dims[0];
            std::vector<double> values;
            values.reserve(groupLength);
            for (indexType group = 0; group < groupCount; ++group) {
                values.clear();
                indexType offset = group * groupLength;
                for (indexType k = 0; k < groupLength; ++k) {
                    double value = xPtr[offset + k];
                    if (!std::isnan(value)) {
                        values.push_back(value);
                    }
                }
                std::sort(values.begin(), values.end());
                indexType base = outputIndex(group, baseDims, baseStrides, valueStrides);
                for (indexType pIndex = 0; pIndex < pCount; ++pIndex) {
                    double value = quantileValue(values, probs[pIndex], plan.method);
                    raw[pIndex * groupCount + group] = value;
                    outPtr[base + pIndex * valueStrides[qDim]] = value;
                }
            }
        } else {
            std::vector<std::vector<double>> groups(groupCount);
            for (indexType k = 0; k < data.elementCount(); ++k) {
                double value = xPtr[k];
                if (!std::isnan(value)) {
                    groups[groupIndex(k, dims, inStrides, baseStrides, opMask)].push_back(value);
                }
            }
            for (indexType group = 0; group < groupCount; ++group) {
                std::sort(groups[group].begin(), groups[group].end());
                indexType base = outputIndex(group, baseDims, baseStrides, valueStrides);
                for (indexType pIndex = 0; pIndex < pCount; ++pIndex) {
                    double value = quantileValue(groups[group], probs[pIndex], plan.method);
                    raw[pIndex * groupCount + group] = value;
                    outPtr[base + pIndex * valueStrides[qDim]] = value;
                }
            }
        }
        QuantileResult result;
        result.raw = std::move(raw);
        result.baseDims = simplifiedDims(baseDims);
        result.valueDims = simplifiedDims(valueDims);
        result.values = ArrayOf(NLS_DOUBLE, result.valueDims, outPtr);
        result.pCount = pCount;
        result.groupCount = groupCount;
        if (x.dataClass() == NLS_SINGLE) {
            result.values.promoteType(NLS_SINGLE);
        }
        return result;
    }
    //=============================================================================
    void
    checkInputData(const ArrayOf& x)
    {
        if (!x.isNumeric() || x.isSparse() || !x.isReal()) {
            Error("Nelson:statistics:aMustBeRealArray", "%s must be a real numeric array.", "A");
        }
    }
    //=============================================================================
    ArrayOf
    iqrRange(const QuantileResult& quantiles, const ArrayOf& x)
    {
        double* ptr
            = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, quantiles.groupCount));
        for (indexType k = 0; k < quantiles.groupCount; ++k) {
            ptr[k] = quantiles.raw[quantiles.groupCount + k] - quantiles.raw[k];
        }
        ArrayOf result(NLS_DOUBLE, quantiles.baseDims, ptr);
        if (x.dataClass() == NLS_SINGLE) {
            result.promoteType(NLS_SINGLE);
        }
        return result;
    }
    //=============================================================================
} // namespace
//=============================================================================
ArrayOfVector
Quantile(const ArrayOfVector& args)
{
    checkInputData(args[0]);
    std::vector<double> probs = quantileProbabilities(args[1]);
    QuantileResult result = computeQuantiles(args[0], probs, parsePlan(args, 2));
    ArrayOfVector outputs;
    outputs << result.values;
    return outputs;
}
//=============================================================================
ArrayOfVector
Prctile(const ArrayOfVector& args)
{
    checkInputData(args[0]);
    std::vector<double> probs = probabilities(args[1], true);
    QuantileResult result = computeQuantiles(args[0], probs, parsePlan(args, 2));
    ArrayOfVector outputs;
    outputs << result.values;
    return outputs;
}
//=============================================================================
ArrayOfVector
Iqr(const ArrayOfVector& args, int nLhs)
{
    checkInputData(args[0]);
    QuantilePlan plan = parsePlan(args, 1);
    QuantileResult result = computeQuantiles(args[0], { 0.25, 0.75 }, plan);
    ArrayOfVector outputs;
    outputs << iqrRange(result, args[0]);
    if (nLhs > 1) {
        outputs << result.values;
    }
    return outputs;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
