//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "StatisticsZScore.hpp"
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
    struct ZScorePlan
    {
        int flag { 0 };
        bool all { false };
        std::vector<indexType> opDims;
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
    int
    flagValue(const ArrayOf& value)
    {
        if (value.isEmpty()) {
            return 0;
        }
        if (!value.isNumeric() || value.isSparse() || !value.isReal() || !value.isScalar()) {
            Error("Nelson:statistics:invalidFlag", "%s must be 0 or 1.", "FLAG");
        }
        double flag = value.asDoubleScalar();
        if (flag != 0 && flag != 1) {
            Error("Nelson:statistics:invalidFlag", "%s must be 0 or 1.", "FLAG");
        }
        return static_cast<int>(flag);
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
    ZScorePlan
    parsePlan(const ArrayOfVector& args)
    {
        ZScorePlan plan;
        if (args.size() > 1) {
            plan.flag = flagValue(args[1]);
        }
        if (args.size() > 2) {
            const ArrayOf& dimArg = args[2];
            if (isTextScalar(dimArg)) {
                if (lowerText(dimArg) != L"all") {
                    Error("Nelson:statistics:invalidDimensionArg", "Dimension argument must be %s.",
                        "a positive integer scalar, vector, or 'all'");
                }
                plan.all = true;
            } else {
                plan.opDims = dimensionVector(dimArg);
            }
        }
        return plan;
    }
    //=============================================================================
    std::vector<indexType>
    inputDims(const Dimensions& dims, const ZScorePlan& plan)
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
    defaultOpDims(const std::vector<indexType>& dims)
    {
        for (indexType k = 0; k < dims.size(); ++k) {
            if (dims[k] != 1) {
                return { k };
            }
        }
        return { 0 };
    }
    //=============================================================================
    std::vector<indexType>
    operationDims(const std::vector<indexType>& dims, const ZScorePlan& plan)
    {
        if (plan.all) {
            std::vector<indexType> values(dims.size());
            std::iota(values.begin(), values.end(), 0);
            return values;
        }
        return plan.opDims.empty() ? defaultOpDims(dims) : plan.opDims;
    }
    //=============================================================================
    Dimensions
    outputDimensions(const std::vector<indexType>& dims, const std::vector<indexType>& opDims)
    {
        std::vector<indexType> out = dims;
        for (indexType dim : opDims) {
            if (dim >= out.size()) {
                out.resize(dim + 1, 1);
            }
            out[dim] = 1;
        }
        Dimensions result(out);
        result.simplify();
        return result;
    }
    //=============================================================================
    std::vector<indexType>
    outputDimsFull(const std::vector<indexType>& dims, const std::vector<indexType>& opDims)
    {
        std::vector<indexType> out = dims;
        for (indexType dim : opDims) {
            if (dim >= out.size()) {
                out.resize(dim + 1, 1);
            }
            out[dim] = 1;
        }
        return out;
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
    indexType
    groupIndex(indexType index, const std::vector<indexType>& dims,
        const std::vector<indexType>& inStrides, const std::vector<indexType>& outStrides,
        const std::vector<bool>& opMask)
    {
        indexType group = 0;
        for (indexType dim = 0; dim < dims.size(); ++dim) {
            indexType coord = (index / inStrides[dim]) % dims[dim];
            if (!opMask[dim]) {
                group += coord * outStrides[dim];
            }
        }
        return group;
    }
    //=============================================================================
    template <class T>
    double
    readValue(const ArrayOf& x, indexType index)
    {
        return static_cast<double>(static_cast<const T*>(x.dataPointer())[index]);
    }
    //=============================================================================
    template <class T>
    void
    writeValue(void* ptr, indexType index, double value)
    {
        static_cast<T*>(ptr)[index] = static_cast<T>(value);
    }
    //=============================================================================
    void*
    allocate(NelsonType type, indexType count)
    {
        return ArrayOf::allocateArrayOf(type, count);
    }
    //=============================================================================
    template <class T>
    ArrayOfVector
    computeZScore(const ArrayOf& x, NelsonType outType, const ZScorePlan& plan)
    {
        std::vector<indexType> dims = inputDims(x.dimensions(), plan);
        std::vector<indexType> opDims = operationDims(dims, plan);
        Dimensions muDims = outputDimensions(dims, opDims);
        std::vector<indexType> outDims = outputDimsFull(dims, opDims);
        indexType elementCount = x.elementCount();
        indexType groupCount = muDims.elementCount();
        std::vector<indexType> inStrides = strides(dims);
        std::vector<indexType> outStrides = strides(outDims);
        std::vector<bool> opMask = operationMask(dims.size(), opDims);
        std::vector<double> sums(groupCount, 0), sumsSquares(groupCount, 0);
        std::vector<indexType> counts(groupCount, 0);
        for (indexType k = 0; k < elementCount; ++k) {
            indexType group = groupIndex(k, dims, inStrides, outStrides, opMask);
            double value = readValue<T>(x, k);
            sums[group] += value;
            sumsSquares[group] += value * value;
            counts[group]++;
        }
        std::vector<double> means(groupCount, std::numeric_limits<double>::quiet_NaN());
        std::vector<double> sigmas(groupCount, std::numeric_limits<double>::quiet_NaN());
        for (indexType k = 0; k < groupCount; ++k) {
            if (counts[k] == 0) {
                continue;
            }
            means[k] = sums[k] / static_cast<double>(counts[k]);
            double denom = plan.flag == 0 ? static_cast<double>(counts[k] - 1)
                                          : static_cast<double>(counts[k]);
            if (denom <= 0) {
                sigmas[k] = 0;
            } else {
                double variance
                    = (sumsSquares[k] - sums[k] * sums[k] / static_cast<double>(counts[k])) / denom;
                sigmas[k] = std::isnan(variance) ? variance : std::sqrt(std::max(0.0, variance));
            }
        }
        void* zPtr = allocate(outType, elementCount);
        void* muPtr = allocate(outType, groupCount);
        void* sigmaPtr = allocate(outType, groupCount);
        for (indexType k = 0; k < groupCount; ++k) {
            writeValue<T>(muPtr, k, means[k]);
            writeValue<T>(sigmaPtr, k, sigmas[k]);
        }
        for (indexType k = 0; k < elementCount; ++k) {
            indexType group = groupIndex(k, dims, inStrides, outStrides, opMask);
            double divisor = sigmas[group] == 0 ? 1 : sigmas[group];
            double z = (readValue<T>(x, k) - means[group]) / divisor;
            writeValue<T>(zPtr, k, z);
        }
        ArrayOfVector out;
        out << ArrayOf(outType, x.dimensions(), zPtr);
        out << ArrayOf(outType, muDims, muPtr);
        out << ArrayOf(outType, muDims, sigmaPtr);
        return out;
    }
    //=============================================================================
} // namespace
//=============================================================================
ArrayOfVector
ZScore(const ArrayOfVector& args, int nLhs)
{
    ArrayOf x = args[0];
    if (!x.isNumeric() || x.isSparse() || !x.isReal()) {
        Error("Nelson:statistics:xMustBeRealArray", "%s must be a real numeric array.", "X");
    }
    ZScorePlan plan = parsePlan(args);
    ArrayOfVector allOutputs;
    if (x.dataClass() == NLS_SINGLE) {
        allOutputs = computeZScore<single>(x, NLS_SINGLE, plan);
    } else {
        x.promoteType(NLS_DOUBLE);
        allOutputs = computeZScore<double>(x, NLS_DOUBLE, plan);
    }
    ArrayOfVector outputs;
    outputs << allOutputs[0];
    if (nLhs > 1) {
        outputs << allOutputs[1];
    }
    if (nLhs > 2) {
        outputs << allOutputs[2];
    }
    return outputs;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
