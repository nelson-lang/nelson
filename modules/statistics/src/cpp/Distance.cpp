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
#include "FunctionDef.hpp"
#include "i18n.hpp"
#include "nlsBuildConfig.h"
#include <Eigen/Dense>
#include <algorithm>
#include <cmath>
#include <limits>
#include <numeric>
#include <vector>
//=============================================================================
namespace Nelson {
//=============================================================================
enum class DistanceMetric
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
    Mahalanobis,
    Spearman
};
//=============================================================================
static bool
isTextScalar(const ArrayOf& value)
{
    return value.isRowVectorCharacterArray() || value.isScalarStringArray();
}
//=============================================================================
static std::wstring
lowerText(const ArrayOf& value)
{
    std::wstring text = value.asWideString();
    std::transform(text.begin(), text.end(), text.begin(), [](wchar_t c) { return towlower(c); });
    return text;
}
//=============================================================================
static bool
isRealNumericMatrix(const ArrayOf& value)
{
    return value.isNumeric() && !value.isSparse() && !value.isComplex()
        && value.dimensions().length() == 2;
}
//=============================================================================
static ArrayOf
asDoubleMatrix(const ArrayOf& value, const std::wstring& message)
{
    if (!isRealNumericMatrix(value)) {
        Error(L"Nelson:statistics:invalidValue", message);
    }
    ArrayOf copy = value;
    copy.promoteType(NLS_DOUBLE);
    return copy;
}
//=============================================================================
static double
matrixValue(const double* x, indexType rows, indexType row, indexType col)
{
    return x[row + col * rows];
}
//=============================================================================
static DistanceMetric
distanceMetric(const ArrayOf& value)
{
    if (!isTextScalar(value)) {
        Error("Nelson:validators:mustBeTextScalar",
            "%s must be a character vector or string scalar.", "Distance");
    }
    std::wstring text = lowerText(value);
    std::vector<std::pair<std::wstring, DistanceMetric>> names {
        { L"euclidean", DistanceMetric::Euclidean },
        { L"fasteuclidean", DistanceMetric::Euclidean },
        { L"squaredeuclidean", DistanceMetric::SquareEuclidean },
        { L"sqeuclidean", DistanceMetric::SquareEuclidean },
        { L"cityblock", DistanceMetric::Cityblock }, { L"chebychev", DistanceMetric::Chebychev },
        { L"chebyshev", DistanceMetric::Chebychev }, { L"cosine", DistanceMetric::Cosine },
        { L"correlation", DistanceMetric::Correlation }, { L"hamming", DistanceMetric::Hamming },
        { L"jaccard", DistanceMetric::Jaccard }, { L"minkowski", DistanceMetric::Minkowski },
        { L"seuclidean", DistanceMetric::StandardizedEuclidean },
        { L"fastseuclidean", DistanceMetric::StandardizedEuclidean },
        { L"mahalanobis", DistanceMetric::Mahalanobis }, { L"spearman", DistanceMetric::Spearman }
    };
    std::vector<DistanceMetric> matches;
    for (const auto& name : names) {
        if (name.first.rfind(text, 0) == 0) {
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
static double
scalarDouble(const ArrayOf& value, const std::wstring& message)
{
    ArrayOf copy = asDoubleMatrix(value, message);
    return copy.asDoubleScalar(false, false);
}
//=============================================================================
static std::vector<double>
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
static std::vector<double>
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
static Eigen::MatrixXd
defaultCovariance(const double* x, indexType rows, indexType columns)
{
    Eigen::MatrixXd cov = Eigen::MatrixXd::Zero(columns, columns);
    if (rows < 2) {
        return cov;
    }
    std::vector<double> means(columns, 0);
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
static Eigen::MatrixXd
covarianceParameter(const ArrayOf& value, indexType columns)
{
    ArrayOf copy = asDoubleMatrix(value,
        formatI18nMessage(Nelson::resolveMessageTemplate(
                              L"Nelson:statistics:numericMatrix", L"%s must be a numeric matrix."),
            L"Distance parameter"));
    if (copy.rows() != columns || copy.columns() != columns) {
        Error("Nelson:statistics:distanceParameterSizeMismatch", "%s must have compatible size.",
            "Distance parameter");
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
static std::vector<double>
rankRows(const double* x, indexType rows, indexType columns)
{
    std::vector<double> ranks(rows * columns);
    std::vector<indexType> order(columns);
    for (indexType r = 0; r < rows; ++r) {
        std::iota(order.begin(), order.end(), 0);
        std::sort(order.begin(), order.end(), [&](indexType a, indexType b) {
            return matrixValue(x, rows, r, a) < matrixValue(x, rows, r, b);
        });
        for (indexType k = 0; k < columns;) {
            indexType j = k + 1;
            while (j < columns
                && matrixValue(x, rows, r, order[j]) == matrixValue(x, rows, r, order[k])) {
                ++j;
            }
            double rank = 0.5 * static_cast<double>(k + j + 1);
            for (indexType n = k; n < j; ++n) {
                ranks[r + order[n] * rows] = rank;
            }
            k = j;
        }
    }
    return ranks;
}
//=============================================================================
static double
safeRatio(double num, double denom)
{
    return denom == 0 ? 0 : num / denom;
}
//=============================================================================
static double
correlationDistance(const double* x, indexType rows, indexType columns, indexType a, indexType b)
{
    double meanA = 0;
    double meanB = 0;
    for (indexType c = 0; c < columns; ++c) {
        meanA += matrixValue(x, rows, a, c);
        meanB += matrixValue(x, rows, b, c);
    }
    meanA /= static_cast<double>(columns);
    meanB /= static_cast<double>(columns);
    double dot = 0;
    double normA = 0;
    double normB = 0;
    for (indexType c = 0; c < columns; ++c) {
        double va = matrixValue(x, rows, a, c) - meanA;
        double vb = matrixValue(x, rows, b, c) - meanB;
        dot += va * vb;
        normA += va * va;
        normB += vb * vb;
    }
    return 1.0 - safeRatio(dot, std::sqrt(normA) * std::sqrt(normB));
}
//=============================================================================
static double
basicDistance(const double* x, indexType rows, indexType columns, indexType a, indexType b,
    DistanceMetric metric, double minkowskiPower, const std::vector<double>& scale)
{
    double acc = 0;
    double sumAbs = 0;
    double maxValue = 0;
    double dot = 0;
    double normA = 0;
    double normB = 0;
    double unequal = 0;
    double nonzero = 0;
    double unequalNonzero = 0;
    for (indexType c = 0; c < columns; ++c) {
        double va = matrixValue(x, rows, a, c);
        double vb = matrixValue(x, rows, b, c);
        double diff = std::abs(va - vb);
        if (metric == DistanceMetric::StandardizedEuclidean) {
            diff /= scale[c];
        }
        acc += metric == DistanceMetric::Minkowski ? std::pow(diff, minkowskiPower) : diff * diff;
        sumAbs += diff;
        maxValue = std::max(maxValue, diff);
        dot += va * vb;
        normA += va * va;
        normB += vb * vb;
        unequal += va != vb ? 1 : 0;
        nonzero += (va != 0 || vb != 0) ? 1 : 0;
        unequalNonzero += (va != vb && (va != 0 || vb != 0)) ? 1 : 0;
    }
    switch (metric) {
    case DistanceMetric::SquareEuclidean:
        return acc;
    case DistanceMetric::Cityblock:
        return sumAbs;
    case DistanceMetric::Chebychev:
        return maxValue;
    case DistanceMetric::Cosine:
        return 1.0 - safeRatio(dot, std::sqrt(normA) * std::sqrt(normB));
    case DistanceMetric::Hamming:
        return unequal / static_cast<double>(columns);
    case DistanceMetric::Jaccard:
        return safeRatio(unequalNonzero, nonzero);
    case DistanceMetric::Minkowski:
        return std::pow(acc, 1.0 / minkowskiPower);
    default:
        return std::sqrt(acc);
    }
}
//=============================================================================
static bool
pairHasNaN(const double* x, indexType rows, indexType columns, indexType a, indexType b)
{
    for (indexType c = 0; c < columns; ++c) {
        if (std::isnan(matrixValue(x, rows, a, c)) || std::isnan(matrixValue(x, rows, b, c))) {
            return true;
        }
    }
    return false;
}
//=============================================================================
static double
mahalanobisDistance(const double* x, indexType rows, indexType columns, indexType a, indexType b,
    const Eigen::MatrixXd& invCov)
{
    Eigen::VectorXd diff(columns);
    for (indexType c = 0; c < columns; ++c) {
        diff(c) = matrixValue(x, rows, a, c) - matrixValue(x, rows, b, c);
    }
    double value = diff.transpose() * invCov * diff;
    return std::sqrt(std::max(0.0, value));
}
//=============================================================================
static void
parsePDistanceOptions(const ArrayOfVector& args, DistanceMetric& metric, double& minkowskiPower,
    std::vector<double>& scale, Eigen::MatrixXd& invCov, const ArrayOf& x)
{
    metric = args.size() > 1 ? distanceMetric(args[1]) : DistanceMetric::Euclidean;
    const double* data = static_cast<const double*>(x.dataPointer());
    indexType rows = x.rows();
    indexType columns = x.columns();
    if (metric == DistanceMetric::Minkowski) {
        minkowskiPower = args.size() > 2
            ? scalarDouble(args[2],
                  formatI18nMessage(Nelson::resolveMessageTemplate(
                                        L"Nelson:validators:mustBeNumeric", L"%s must be numeric."),
                      L"Distance parameter"))
            : 2.0;
        if (!(minkowskiPower > 0)) {
            Error("Nelson:validators:mustBePositive", "%s must be positive.", "Distance parameter");
        }
    } else if (metric == DistanceMetric::StandardizedEuclidean) {
        scale = args.size() > 2
            ? vectorParameter(args[2], columns,
                  formatI18nMessage(Nelson::resolveMessageTemplate(
                                        L"Nelson:validators:mustBeVector", L"%s must be a vector."),
                      L"Distance parameter"))
            : defaultScale(data, rows, columns);
    } else if (metric == DistanceMetric::Mahalanobis) {
        Eigen::MatrixXd cov = args.size() > 2 ? covarianceParameter(args[2], columns)
                                              : defaultCovariance(data, rows, columns);
        invCov = cov.inverse();
    } else if (args.size() > 2) {
        Error("Nelson:statistics:distanceParameterNotSupported",
            "Distance parameter is not supported for this metric.");
    }
}
//=============================================================================
ArrayOf
PDistance(const ArrayOfVector& args)
{
    ArrayOf x = asDoubleMatrix(args[0],
        formatI18nMessage(Nelson::resolveMessageTemplate(L"Nelson:statistics:realNumericMatrix",
                              L"%s must be a real numeric matrix."),
            L"X"));
    indexType rows = x.rows();
    indexType columns = x.columns();
    indexType count = rows * (rows - 1) / 2;
    double* out = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, count));
    DistanceMetric metric;
    double minkowskiPower = 2.0;
    std::vector<double> scale;
    Eigen::MatrixXd invCov;
    parsePDistanceOptions(args, metric, minkowskiPower, scale, invCov, x);
    const double* data = static_cast<const double*>(x.dataPointer());
    std::vector<double> ranks;
    const double* distanceData = data;
    if (metric == DistanceMetric::Spearman) {
        ranks = rankRows(data, rows, columns);
        distanceData = ranks.data();
        metric = DistanceMetric::Correlation;
    }
#if WITH_OPENMP
#pragma omp parallel for if (count > OMP_DEFAULT_THRESHOLD)
#endif
    for (ompIndexType idx = 0; idx < (ompIndexType)count; ++idx) {
        indexType k = static_cast<indexType>(idx);
        indexType col = static_cast<indexType>((1 + std::sqrt(1 + 8.0 * k)) / 2);
        while (col * (col + 1) / 2 <= k) {
            ++col;
        }
        indexType base = col * (col - 1) / 2;
        indexType row = k - base;
        indexType a = row;
        indexType b = col;
        if (pairHasNaN(data, rows, columns, a, b)) {
            out[k] = std::numeric_limits<double>::quiet_NaN();
        } else if (metric == DistanceMetric::Correlation) {
            out[k] = correlationDistance(distanceData, rows, columns, a, b);
        } else if (metric == DistanceMetric::Mahalanobis) {
            out[k] = mahalanobisDistance(data, rows, columns, a, b, invCov);
        } else {
            out[k]
                = basicDistance(distanceData, rows, columns, a, b, metric, minkowskiPower, scale);
        }
    }
    return ArrayOf(NLS_DOUBLE, Dimensions(1, count), out);
}
//=============================================================================
static std::wstring
squareformDirection(const ArrayOfVector& args, bool isVector)
{
    if (args.size() < 2) {
        return isVector ? L"tomatrix" : L"tovector";
    }
    if (!isTextScalar(args[1])) {
        Error("Nelson:validators:mustBeTextScalar",
            "%s must be a character vector or string scalar.", "Direction");
    }
    std::wstring text = lowerText(args[1]);
    std::vector<std::wstring> matches;
    std::vector<std::wstring> names { L"tomatrix", L"tovector" };
    for (const auto& name : names) {
        if (name.rfind(text, 0) == 0) {
            matches.push_back(name);
        }
    }
    if (matches.empty() || matches.size() > 1) {
        Error("Nelson:statistics:invalidDirectionValue", "Invalid %s value.", "direction");
    }
    return matches[0];
}
//=============================================================================
static indexType
squareSizeFromVector(indexType count)
{
    double n = (1.0 + std::sqrt(1.0 + 8.0 * static_cast<double>(count))) / 2.0;
    indexType size = static_cast<indexType>(std::floor(n + 0.5));
    if (size * (size - 1) / 2 != count) {
        Error("Nelson:statistics:invalidDistanceVectorLength",
            "Distance vector length must be a binomial coefficient.");
    }
    return size;
}
//=============================================================================
static ArrayOf
vectorToMatrix(const ArrayOf& vector)
{
    indexType n = squareSizeFromVector(vector.elementCount());
    double* out = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, n * n));
    std::fill(out, out + n * n, 0.0);
    const double* data = static_cast<const double*>(vector.dataPointer());
    indexType k = 0;
    for (indexType c = 1; c < n; ++c) {
        for (indexType r = 0; r < c; ++r) {
            out[r + c * n] = data[k];
            out[c + r * n] = data[k];
            ++k;
        }
    }
    return ArrayOf(NLS_DOUBLE, Dimensions(n, n), out);
}
//=============================================================================
static ArrayOf
matrixToVector(const ArrayOf& matrix)
{
    if (matrix.rows() != matrix.columns()) {
        Error("Nelson:expectedSquare", "%s must be square.", "Distance matrix");
    }
    indexType n = matrix.rows();
    indexType count = n * (n - 1) / 2;
    double* out = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, count));
    const double* data = static_cast<const double*>(matrix.dataPointer());
    indexType k = 0;
    for (indexType c = 1; c < n; ++c) {
        for (indexType r = 0; r < c; ++r) {
            if (data[r + c * n] != data[c + r * n]) {
                Error("Nelson:statistics:distanceMatrixNotSymmetric", "%s must be symmetric.",
                    "Distance matrix");
            }
            out[k++] = data[r + c * n];
        }
    }
    return ArrayOf(NLS_DOUBLE, Dimensions(1, count), out);
}
//=============================================================================
ArrayOf
SquareForm(const ArrayOfVector& args)
{
    ArrayOf value = asDoubleMatrix(args[0],
        formatI18nMessage(
            Nelson::resolveMessageTemplate(L"Nelson:statistics:realNumericVectorMatrix",
                L"%s must be a real numeric vector or matrix."),
            L"Input"));
    bool vectorInput = value.isVector() && (value.rows() == 1 || value.columns() == 1);
    std::wstring direction = squareformDirection(args, vectorInput);
    if (direction == L"tomatrix") {
        if (!vectorInput) {
            Error("Nelson:validators:mustBeVector", "%s must be a vector.", "Value");
        }
        return vectorToMatrix(value);
    }
    return matrixToVector(value);
}
//=============================================================================
struct KNNOptions
{
    indexType k { 1 };
    DistanceMetric metric { DistanceMetric::Euclidean };
    bool includeTies { false };
    double minkowskiPower { 2.0 };
    std::vector<double> scale;
    Eigen::MatrixXd invCov;
};
//=============================================================================
struct Neighbor
{
    indexType index;
    double distance;
};
//=============================================================================
static bool
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
static indexType
positiveIntegerScalar(const ArrayOf& value, const std::wstring& message)
{
    if (!value.isNumeric() || value.isSparse() || !value.isReal() || !value.isScalar()) {
        Error(L"Nelson:statistics:invalidValue", message);
    }
    double scalar = value.asDoubleScalar();
    if (!std::isfinite(scalar) || scalar < 1 || std::floor(scalar) != scalar) {
        Error(L"Nelson:statistics:invalidValue", message);
    }
    return static_cast<indexType>(scalar);
}
//=============================================================================
static void
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
static void
parseKNNDistanceParameter(
    KNNOptions& opts, const std::wstring& name, const ArrayOf& value, indexType columns)
{
    if (name == L"p") {
        opts.minkowskiPower = scalarDouble(value,
            formatI18nMessage(Nelson::resolveMessageTemplate(L"Nelson:statistics:mustBePositive",
                                  L"%s must be a positive scalar."),
                L"P"));
        if (!(opts.minkowskiPower > 0)) {
            Error("Nelson:statistics:pMustBePositive", "%s must be a positive scalar.", "P");
        }
    } else if (name == L"scale") {
        opts.scale = vectorParameter(value, columns,
            resolveMessageTemplate(L"Nelson:statistics:scaleVectorOnePer",
                L"Scale must be a vector with one value per column."));
    } else if (name == L"cov") {
        opts.invCov = covarianceParameter(value, columns).inverse();
    }
}
//=============================================================================
static std::wstring
knnOptionName(const ArrayOf& value)
{
    if (!isTextScalar(value)) {
        Error("Nelson:validators:mustBeTextScalar",
            "%s must be a character vector or string scalar.", "Name");
    }
    std::wstring text = lowerText(value);
    std::vector<std::wstring> names { L"k", L"distance", L"includeties", L"nsmethod",
        L"sortindices", L"p", L"scale", L"cov", L"bucketsize", L"cachesize" };
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
static KNNOptions
parseKNNOptions(const ArrayOfVector& args, indexType columns)
{
    KNNOptions opts;
    for (size_t k = 2; k < args.size(); k += 2) {
        std::wstring name = knnOptionName(args[k]);
        const ArrayOf& value = args[k + 1];
        if (name == L"k") {
            opts.k = positiveIntegerScalar(value,
                formatI18nMessage(
                    Nelson::resolveMessageTemplate(L"Nelson:statistics:mustBePositiveInteger",
                        L"%s must be a positive integer scalar."),
                    L"K"));
        } else if (name == L"distance") {
            opts.metric = distanceMetric(value);
        } else if (name == L"includeties") {
            opts.includeTies = scalarBoolean(value,
                formatI18nMessage(
                    Nelson::resolveMessageTemplate(
                        L"Nelson:statistics:mustBeTrueOrFalse", L"%s must be true or false."),
                    L"IncludeTies"));
        } else if (name == L"nsmethod") {
            validateNSMethod(value);
        } else if (name == L"sortindices" || name == L"bucketsize" || name == L"cachesize") {
            if (name == L"sortindices") {
                scalarBoolean(value,
                    formatI18nMessage(
                        Nelson::resolveMessageTemplate(
                            L"Nelson:statistics:mustBeTrueOrFalse", L"%s must be true or false."),
                        L"SortIndices"));
            } else {
                positiveIntegerScalar(value,
                    formatI18nMessage(
                        Nelson::resolveMessageTemplate(L"Nelson:statistics:mustBePositiveInteger",
                            L"%s must be a positive integer scalar."),
                        L"Option value"));
            }
        } else {
            parseKNNDistanceParameter(opts, name, value, columns);
        }
    }
    return opts;
}
//=============================================================================
static double
matrixDistance(const double* x, indexType xRows, const double* y, indexType yRows,
    indexType columns, indexType xRow, indexType yRow, DistanceMetric metric,
    const KNNOptions& opts)
{
    double acc = 0, sumAbs = 0, maxValue = 0, dot = 0, normX = 0, normY = 0;
    double unequal = 0, nonzero = 0, unequalNonzero = 0, meanX = 0, meanY = 0;
    if (metric == DistanceMetric::Correlation) {
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
        double vx = metric == DistanceMetric::Correlation ? xv - meanX : xv;
        double vy = metric == DistanceMetric::Correlation ? yv - meanY : yv;
        double diff = std::abs(xv - yv);
        if (metric == DistanceMetric::StandardizedEuclidean) {
            diff /= opts.scale[c];
        }
        acc += metric == DistanceMetric::Minkowski ? std::pow(diff, opts.minkowskiPower)
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
    switch (metric) {
    case DistanceMetric::SquareEuclidean:
        return acc;
    case DistanceMetric::Cityblock:
        return sumAbs;
    case DistanceMetric::Chebychev:
        return maxValue;
    case DistanceMetric::Cosine:
    case DistanceMetric::Correlation:
        return 1.0 - safeRatio(dot, std::sqrt(normX) * std::sqrt(normY));
    case DistanceMetric::Hamming:
        return unequal / static_cast<double>(columns);
    case DistanceMetric::Jaccard:
        return safeRatio(unequalNonzero, nonzero);
    case DistanceMetric::Minkowski:
        return std::pow(acc, 1.0 / opts.minkowskiPower);
    default:
        return std::sqrt(acc);
    }
}
//=============================================================================
static double
mahalanobisXY(const double* x, indexType xRows, const double* y, indexType yRows, indexType columns,
    indexType xRow, indexType yRow, const Eigen::MatrixXd& invCov)
{
    Eigen::VectorXd diff(columns);
    for (indexType c = 0; c < columns; ++c) {
        diff(c) = matrixValue(x, xRows, xRow, c) - matrixValue(y, yRows, yRow, c);
    }
    double value = diff.transpose() * invCov * diff;
    return std::sqrt(std::max(0.0, value));
}
//=============================================================================
static std::vector<Neighbor>
queryNeighbors(const double* x, indexType xRows, const double* y, indexType yRows,
    indexType columns, indexType yRow, const KNNOptions& opts)
{
    std::vector<Neighbor> values;
    values.reserve(xRows);
    for (indexType r = 0; r < xRows; ++r) {
        double d = opts.metric == DistanceMetric::Mahalanobis
            ? mahalanobisXY(x, xRows, y, yRows, columns, r, yRow, opts.invCov)
            : matrixDistance(x, xRows, y, yRows, columns, r, yRow, opts.metric, opts);
        values.push_back({ r + 1, d });
    }
    std::stable_sort(values.begin(), values.end(), [](const Neighbor& a, const Neighbor& b) {
        if (a.distance == b.distance) {
            return a.index < b.index;
        }
        return a.distance < b.distance;
    });
    return values;
}
//=============================================================================
static void
fillKNNMatrix(double* idx, double* dist, indexType yRows, indexType yRow,
    const std::vector<Neighbor>& neighbors, indexType k, bool needDistance)
{
    for (indexType c = 0; c < k; ++c) {
        idx[yRow + c * yRows] = static_cast<double>(neighbors[c].index);
        if (needDistance) {
            dist[yRow + c * yRows] = neighbors[c].distance;
        }
    }
}
//=============================================================================
static ArrayOf
cellColumnFromNeighbors(const std::vector<std::vector<Neighbor>>& rows, bool distances)
{
    ArrayOfMatrix matrix;
    matrix.reserve(rows.size());
    for (const auto& rowValues : rows) {
        double* data = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, rowValues.size()));
        for (indexType k = 0; k < rowValues.size(); ++k) {
            data[k] = distances ? rowValues[k].distance : static_cast<double>(rowValues[k].index);
        }
        ArrayOfVector row;
        row << ArrayOf(NLS_DOUBLE, Dimensions(1, static_cast<indexType>(rowValues.size())), data);
        matrix.push_back(row);
    }
    return ArrayOf::cellArray(matrix);
}
//=============================================================================
static std::vector<Neighbor>
neighborsWithTies(const std::vector<Neighbor>& sorted, indexType k)
{
    double limit = sorted[k - 1].distance;
    std::vector<Neighbor> values;
    for (const auto& item : sorted) {
        if (item.distance <= limit) {
            values.push_back(item);
        }
    }
    return values;
}
//=============================================================================
ArrayOfVector
KNNSearch(const ArrayOfVector& args, int nLhs)
{
    if ((args.size() - 2) % 2 != 0) {
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
    if (x.columns() != y.columns()) {
        Error(
            "Nelson:statistics:xyColumnMismatch", "X and Y must have the same number of columns.");
    }
    KNNOptions opts = parseKNNOptions(args, x.columns());
    if (opts.k > x.rows()) {
        Error("Nelson:statistics:kTooLarge", "K must not exceed the number of rows in X.");
    }
    if (opts.metric == DistanceMetric::StandardizedEuclidean && opts.scale.empty()) {
        opts.scale
            = defaultScale(static_cast<const double*>(x.dataPointer()), x.rows(), x.columns());
    }
    if (opts.metric == DistanceMetric::Mahalanobis && opts.invCov.size() == 0) {
        opts.invCov
            = defaultCovariance(static_cast<const double*>(x.dataPointer()), x.rows(), x.columns())
                  .inverse();
    }
    const double* xData = static_cast<const double*>(x.dataPointer());
    const double* yData = static_cast<const double*>(y.dataPointer());
    ArrayOfVector output;
    bool needDistance = nLhs > 1;
    if (opts.includeTies) {
        std::vector<std::vector<Neighbor>> tied(y.rows());
        for (indexType r = 0; r < y.rows(); ++r) {
            tied[r] = neighborsWithTies(
                queryNeighbors(xData, x.rows(), yData, y.rows(), x.columns(), r, opts), opts.k);
        }
        output << cellColumnFromNeighbors(tied, false);
        if (needDistance) {
            output << cellColumnFromNeighbors(tied, true);
        }
    } else {
        double* idx = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, y.rows() * opts.k));
        double* dist = needDistance
            ? static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, y.rows() * opts.k))
            : nullptr;
#if WITH_OPENMP
#pragma omp parallel for if (y.rows() * x.rows() > OMP_DEFAULT_THRESHOLD)
#endif
        for (ompIndexType r = 0; r < (ompIndexType)y.rows(); ++r) {
            auto neighbors = queryNeighbors(
                xData, x.rows(), yData, y.rows(), x.columns(), static_cast<indexType>(r), opts);
            fillKNNMatrix(
                idx, dist, y.rows(), static_cast<indexType>(r), neighbors, opts.k, needDistance);
        }
        output << ArrayOf(NLS_DOUBLE, Dimensions(y.rows(), opts.k), idx);
        if (needDistance) {
            output << ArrayOf(NLS_DOUBLE, Dimensions(y.rows(), opts.k), dist);
        }
    }
    return output;
}
//=============================================================================
#include "PDist2Impl.hpp"
//=============================================================================
} // namespace Nelson
//=============================================================================
