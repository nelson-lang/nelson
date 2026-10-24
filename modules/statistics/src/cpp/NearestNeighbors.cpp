//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "StatisticsDistances.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include <Eigen/Dense>
#include <algorithm>
#include <cmath>
#include <limits>
#include <vector>
//=============================================================================
namespace Nelson {
//=============================================================================
namespace {
    //=============================================================================
    enum class RangeMetric
    {
        Euclidean,
        SquareEuclidean,
        Cityblock,
        Chebychev,
        Cosine,
        Correlation,
        Hamming,
        Jaccard,
        Minkowski,
        StandardizedEuclidean,
        Mahalanobis
    };
    //=============================================================================
    struct RangeOptions
    {
        RangeMetric metric { RangeMetric::Euclidean };
        bool sortIndices { true };
        double minkowskiPower { 2.0 };
        std::vector<double> scale;
        Eigen::MatrixXd invCov;
    };
    //=============================================================================
    struct RangeNeighbor
    {
        indexType index;
        double distance;
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
    ArrayOf
    asDoubleMatrix(const ArrayOf& value, const std::wstring& message)
    {
        if (!value.isNumeric() || value.isSparse() || !value.isReal() || !value.is2D()) {
            Error(L"Nelson:statistics:invalidValue", message);
        }
        ArrayOf copy = value;
        copy.promoteType(NLS_DOUBLE);
        return copy;
    }
    //=============================================================================
    double
    matrixValue(const double* x, indexType rows, indexType row, indexType col)
    {
        return x[row + col * rows];
    }
    //=============================================================================
    double
    safeRatio(double num, double denom)
    {
        return denom == 0 ? 0 : num / denom;
    }
    //=============================================================================
    bool
    scalarBoolean(const ArrayOf& value, const std::wstring& message)
    {
        if (value.isLogical() && value.isScalar()) {
            return value.asLogicalScalar() != 0;
        }
        if (value.isNumeric() && value.isScalar()) {
            double scalar = value.asDoubleScalar();
            if (scalar == 0 || scalar == 1) {
                return scalar != 0;
            }
        }
        Error(L"Nelson:statistics:invalidValue", message);
        return false;
    }
    //=============================================================================
    double
    scalarDouble(const ArrayOf& value, const std::wstring& message)
    {
        if (!value.isNumeric() || value.isSparse() || !value.isReal() || !value.isScalar()) {
            Error(L"Nelson:statistics:invalidValue", message);
        }
        return value.asDoubleScalar();
    }
    //=============================================================================
    indexType
    positiveIntegerScalar(const ArrayOf& value, const std::wstring& message)
    {
        double scalar = scalarDouble(value, message);
        if (!std::isfinite(scalar) || scalar < 1 || std::floor(scalar) != scalar) {
            Error(L"Nelson:statistics:invalidValue", message);
        }
        return static_cast<indexType>(scalar);
    }
    //=============================================================================
    RangeMetric
    rangeMetric(const ArrayOf& value)
    {
        if (!isTextScalar(value)) {
            Error("Nelson:validators:mustBeTextScalar",
                "%s must be a character vector or string scalar.", "Distance");
        }
        std::wstring text = lowerText(value);
        std::vector<std::pair<std::wstring, RangeMetric>> names { { L"euclidean",
                                                                      RangeMetric::Euclidean },
            { L"squaredeuclidean", RangeMetric::SquareEuclidean },
            { L"sqeuclidean", RangeMetric::SquareEuclidean },
            { L"cityblock", RangeMetric::Cityblock }, { L"chebychev", RangeMetric::Chebychev },
            { L"cosine", RangeMetric::Cosine }, { L"correlation", RangeMetric::Correlation },
            { L"hamming", RangeMetric::Hamming }, { L"jaccard", RangeMetric::Jaccard },
            { L"minkowski", RangeMetric::Minkowski },
            { L"seuclidean", RangeMetric::StandardizedEuclidean },
            { L"mahalanobis", RangeMetric::Mahalanobis } };
        std::vector<RangeMetric> matches;
        for (const auto& name : names) {
            if (!text.empty() && name.first.rfind(text, 0) == 0) {
                matches.push_back(name.second);
            }
        }
        if (matches.empty()) {
            Error("Nelson:statistics:invalidDistance", "Invalid %s value.", "Distance");
        }
        if (matches.size() > 1) {
            Error("Nelson:statistics:ambiguousValue", "Ambiguous %s value.", "Distance");
        }
        return matches[0];
    }
    //=============================================================================
    std::vector<double>
    vectorParameter(const ArrayOf& value, indexType columns, const std::wstring& message)
    {
        ArrayOf copy = asDoubleMatrix(value, message);
        if (!copy.isVector() || copy.elementCount() != columns) {
            Error(L"Nelson:statistics:invalidValue", message);
        }
        const double* ptr = static_cast<const double*>(copy.dataPointer());
        return std::vector<double>(ptr, ptr + columns);
    }
    //=============================================================================
    std::vector<double>
    defaultScale(const double* x, indexType rows, indexType columns)
    {
        std::vector<double> scale(columns, std::numeric_limits<double>::quiet_NaN());
        for (indexType c = 0; c < columns; ++c) {
            double sum = 0;
            double sumsq = 0;
            for (indexType r = 0; r < rows; ++r) {
                double v = matrixValue(x, rows, r, c);
                sum += v;
                sumsq += v * v;
            }
            if (rows > 1) {
                double var = (sumsq - sum * sum / static_cast<double>(rows)) / (rows - 1);
                scale[c] = std::sqrt(std::max(0.0, var));
            }
        }
        return scale;
    }
    //=============================================================================
    Eigen::MatrixXd
    covarianceParameter(const ArrayOf& value, indexType columns)
    {
        ArrayOf copy = asDoubleMatrix(value,
            formatI18nMessage(Nelson::resolveMessageTemplate(L"Nelson:statistics:numericMatrix",
                                  L"%s must be a numeric matrix."),
                L"Cov"));
        if (copy.rows() != columns || copy.columns() != columns) {
            Error("Nelson:statistics:covSizeMismatch", "%s must have compatible size.", "Cov");
        }
        const double* ptr = static_cast<const double*>(copy.dataPointer());
        Eigen::MatrixXd cov(columns, columns);
        for (indexType c = 0; c < columns; ++c) {
            for (indexType r = 0; r < columns; ++r) {
                cov(r, c) = matrixValue(ptr, columns, r, c);
            }
        }
        return cov;
    }
    //=============================================================================
    Eigen::MatrixXd
    defaultCovariance(const double* x, indexType rows, indexType columns)
    {
        Eigen::MatrixXd cov = Eigen::MatrixXd::Zero(columns, columns);
        std::vector<double> means(columns, 0);
        if (rows < 2) {
            return cov;
        }
        for (indexType c = 0; c < columns; ++c) {
            for (indexType r = 0; r < rows; ++r) {
                means[c] += matrixValue(x, rows, r, c);
            }
            means[c] /= static_cast<double>(rows);
        }
        for (indexType c1 = 0; c1 < columns; ++c1) {
            for (indexType c2 = c1; c2 < columns; ++c2) {
                double value = 0;
                for (indexType r = 0; r < rows; ++r) {
                    value += (matrixValue(x, rows, r, c1) - means[c1])
                        * (matrixValue(x, rows, r, c2) - means[c2]);
                }
                value /= static_cast<double>(rows - 1);
                cov(c1, c2) = value;
                cov(c2, c1) = value;
            }
        }
        return cov;
    }
    //=============================================================================
    std::wstring
    rangeOptionName(const ArrayOf& value)
    {
        if (!isTextScalar(value)) {
            Error("Nelson:validators:mustBeTextScalar",
                "%s must be a character vector or string scalar.", "Name");
        }
        std::wstring text = lowerText(value);
        std::vector<std::wstring> names { L"distance", L"nsmethod", L"sortindices", L"p", L"scale",
            L"cov", L"bucketsize", L"cachesize" };
        std::vector<std::wstring> matches;
        for (const auto& name : names) {
            if (!text.empty() && name.rfind(text, 0) == 0) {
                matches.push_back(name);
            }
        }
        if (matches.empty()) {
            Error("Nelson:statistics:unknownNameValue", "Invalid name-value argument name.");
        }
        if (matches.size() > 1) {
            Error("Nelson:statistics:ambiguousNameValue", "Ambiguous name-value argument name.");
        }
        return matches[0];
    }
    //=============================================================================
    void
    validateNSMethod(const ArrayOf& value)
    {
        if (!isTextScalar(value)) {
            Error("Nelson:validators:mustBeTextScalar",
                "%s must be a character vector or string scalar.", "NSMethod");
        }
        std::wstring text = lowerText(value);
        bool exhaustive = !text.empty() && std::wstring(L"exhaustive").rfind(text, 0) == 0;
        bool kdtree = !text.empty() && std::wstring(L"kdtree").rfind(text, 0) == 0;
        if (!exhaustive && !kdtree) {
            Error("Nelson:statistics:invalidNSMethod", "Invalid %s value.", "NSMethod");
        }
    }
    //=============================================================================
    RangeOptions
    parseRangeOptions(const ArrayOfVector& args, indexType columns)
    {
        RangeOptions opts;
        for (size_t k = 3; k < args.size(); k += 2) {
            std::wstring name = rangeOptionName(args[k]);
            const ArrayOf& value = args[k + 1];
            if (name == L"distance") {
                opts.metric = rangeMetric(value);
            } else if (name == L"sortindices") {
                opts.sortIndices = scalarBoolean(value,
                    formatI18nMessage(
                        Nelson::resolveMessageTemplate(
                            L"Nelson:statistics:mustBeTrueOrFalse", L"%s must be true or false."),
                        L"SortIndices"));
            } else if (name == L"nsmethod") {
                validateNSMethod(value);
            } else if (name == L"p") {
                opts.minkowskiPower = scalarDouble(value,
                    formatI18nMessage(
                        Nelson::resolveMessageTemplate(
                            L"Nelson:statistics:mustBePositive", L"%s must be a positive scalar."),
                        L"P"));
                if (!(opts.minkowskiPower > 0)) {
                    Error(
                        "Nelson:statistics:pMustBePositive", "%s must be a positive scalar.", "P");
                }
            } else if (name == L"scale") {
                opts.scale = vectorParameter(value, columns,
                    resolveMessageTemplate(L"Nelson:statistics:scaleVectorOnePer",
                        L"Scale must be a vector with one value per column."));
            } else if (name == L"cov") {
                opts.invCov = covarianceParameter(value, columns).inverse();
            } else {
                positiveIntegerScalar(value,
                    formatI18nMessage(
                        Nelson::resolveMessageTemplate(L"Nelson:statistics:mustBePositiveInteger",
                            L"%s must be a positive integer scalar."),
                        L"Option value"));
            }
        }
        return opts;
    }
    //=============================================================================
    double
    rangeDistance(const double* x, indexType xRows, const double* y, indexType yRows,
        indexType columns, indexType xRow, indexType yRow, const RangeOptions& opts)
    {
        double acc = 0, sumAbs = 0, maxValue = 0, dot = 0, normX = 0, normY = 0;
        double unequal = 0, nonzero = 0, unequalNonzero = 0, meanX = 0, meanY = 0;
        if (opts.metric == RangeMetric::Correlation) {
            for (indexType c = 0; c < columns; ++c) {
                meanX += matrixValue(x, xRows, xRow, c);
                meanY += matrixValue(y, yRows, yRow, c);
            }
            meanX /= static_cast<double>(columns);
            meanY /= static_cast<double>(columns);
        }
        for (indexType c = 0; c < columns; ++c) {
            double xv = matrixValue(x, xRows, xRow, c);
            double yv = matrixValue(y, yRows, yRow, c);
            double vx = opts.metric == RangeMetric::Correlation ? xv - meanX : xv;
            double vy = opts.metric == RangeMetric::Correlation ? yv - meanY : yv;
            double diff = std::abs(xv - yv);
            if (opts.metric == RangeMetric::StandardizedEuclidean) {
                diff /= opts.scale[c];
            }
            acc += opts.metric == RangeMetric::Minkowski ? std::pow(diff, opts.minkowskiPower)
                                                         : diff * diff;
            sumAbs += diff;
            maxValue = std::max(maxValue, diff);
            dot += vx * vy;
            normX += vx * vx;
            normY += vy * vy;
            unequal += xv != yv ? 1 : 0;
            nonzero += (xv != 0 || yv != 0) ? 1 : 0;
            unequalNonzero += (xv != yv && (xv != 0 || yv != 0)) ? 1 : 0;
        }
        if (opts.metric == RangeMetric::Mahalanobis) {
            Eigen::VectorXd delta(columns);
            for (indexType c = 0; c < columns; ++c) {
                delta(c) = matrixValue(x, xRows, xRow, c) - matrixValue(y, yRows, yRow, c);
            }
            double value = delta.transpose() * opts.invCov * delta;
            return std::sqrt(std::max(0.0, value));
        }
        switch (opts.metric) {
        case RangeMetric::SquareEuclidean:
            return acc;
        case RangeMetric::Cityblock:
            return sumAbs;
        case RangeMetric::Chebychev:
            return maxValue;
        case RangeMetric::Cosine:
        case RangeMetric::Correlation:
            return 1.0 - safeRatio(dot, std::sqrt(normX) * std::sqrt(normY));
        case RangeMetric::Hamming:
            return unequal / static_cast<double>(columns);
        case RangeMetric::Jaccard:
            return safeRatio(unequalNonzero, nonzero);
        case RangeMetric::Minkowski:
            return std::pow(acc, 1.0 / opts.minkowskiPower);
        default:
            return std::sqrt(acc);
        }
    }
    //=============================================================================
    std::vector<RangeNeighbor>
    rangeNeighbors(const double* x, indexType xRows, const double* y, indexType yRows,
        indexType columns, indexType yRow, double radius, const RangeOptions& opts)
    {
        std::vector<RangeNeighbor> values;
        for (indexType r = 0; r < xRows; ++r) {
            double d = rangeDistance(x, xRows, y, yRows, columns, r, yRow, opts);
            if (d <= radius) {
                values.push_back({ r + 1, d });
            }
        }
        if (opts.sortIndices) {
            std::stable_sort(
                values.begin(), values.end(), [](const RangeNeighbor& a, const RangeNeighbor& b) {
                    if (a.distance == b.distance) {
                        return a.index < b.index;
                    }
                    return a.distance < b.distance;
                });
        }
        return values;
    }
    //=============================================================================
    ArrayOf
    cellColumnFromRangeNeighbors(
        const std::vector<std::vector<RangeNeighbor>>& rows, bool distances)
    {
        ArrayOfMatrix matrix;
        matrix.reserve(rows.size());
        for (const auto& rowValues : rows) {
            double* data
                = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, rowValues.size()));
            for (indexType k = 0; k < rowValues.size(); ++k) {
                data[k]
                    = distances ? rowValues[k].distance : static_cast<double>(rowValues[k].index);
            }
            ArrayOfVector row;
            row << ArrayOf(
                NLS_DOUBLE, Dimensions(1, static_cast<indexType>(rowValues.size())), data);
            matrix.push_back(row);
        }
        return ArrayOf::cellArray(matrix);
    }
    //=============================================================================
} // namespace
//=============================================================================
ArrayOfVector
RangeSearch(const ArrayOfVector& args, int nLhs)
{
    if ((args.size() - 3) % 2 != 0) {
        Error("Nelson:statistics:nameValueNotPaired", "Name-value arguments must be paired.");
    }
    ArrayOf x = asDoubleMatrix(args[0],
        formatI18nMessage(Nelson::resolveMessageTemplate(L"Nelson:statistics:realNumericMatrix",
                              L"%s must be a real numeric matrix."),
            L"X"));
    ArrayOf y = asDoubleMatrix(args[1],
        formatI18nMessage(Nelson::resolveMessageTemplate(L"Nelson:statistics:realNumericMatrix",
                              L"%s must be a real numeric matrix."),
            L"Y"));
    double radius = scalarDouble(args[2],
        formatI18nMessage(Nelson::resolveMessageTemplate(
                              L"Nelson:statistics:invalidR", L"%s must be a nonnegative scalar."),
            L"R"));
    if (!std::isfinite(radius) || radius < 0) {
        Error("Nelson:statistics:invalidR", "%s must be a nonnegative scalar.", "R");
    }
    if (x.columns() != y.columns()) {
        Error(
            "Nelson:statistics:xyColumnMismatch", "X and Y must have the same number of columns.");
    }
    RangeOptions opts = parseRangeOptions(args, x.columns());
    if (opts.metric == RangeMetric::StandardizedEuclidean && opts.scale.empty()) {
        opts.scale
            = defaultScale(static_cast<const double*>(x.dataPointer()), x.rows(), x.columns());
    }
    if (opts.metric == RangeMetric::Mahalanobis && opts.invCov.size() == 0) {
        opts.invCov
            = defaultCovariance(static_cast<const double*>(x.dataPointer()), x.rows(), x.columns())
                  .inverse();
    }
    const double* xData = static_cast<const double*>(x.dataPointer());
    const double* yData = static_cast<const double*>(y.dataPointer());
    std::vector<std::vector<RangeNeighbor>> rows(y.rows());
    for (indexType r = 0; r < y.rows(); ++r) {
        rows[r] = rangeNeighbors(xData, x.rows(), yData, y.rows(), x.columns(), r, radius, opts);
    }
    ArrayOfVector output;
    output << cellColumnFromRangeNeighbors(rows, false);
    if (nLhs > 1) {
        output << cellColumnFromRangeNeighbors(rows, true);
    }
    return output;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
