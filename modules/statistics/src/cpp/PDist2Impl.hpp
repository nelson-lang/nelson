//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
struct PDist2Options
{
    DistanceMetric metric { DistanceMetric::Euclidean };
    bool customDistance { false };
    bool selectSmallest { false };
    bool selectLargest { false };
    indexType selectCount { 0 };
    double minkowskiPower { 2.0 };
    std::vector<double> scale;
    Eigen::MatrixXd invCov;
    FunctionDef* customFunction { nullptr };
};
//=============================================================================
static bool
isPDist2OptionName(const ArrayOf& value)
{
    if (!isTextScalar(value)) {
        return false;
    }
    std::wstring text = lowerText(value);
    return text == L"smallest" || text == L"largest" || text == L"cachesize";
}
//=============================================================================
static std::wstring
pdist2OptionName(const ArrayOf& value)
{
    if (!isTextScalar(value)) {
        Error("Nelson:validators:mustBeTextScalar",
            "%s must be a character vector or string scalar.", "Name");
    }
    std::wstring text = lowerText(value);
    std::vector<std::wstring> names { L"smallest", L"largest", L"cachesize" };
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
static bool
rowHasNaN(const double* data, indexType rows, indexType row, indexType columns)
{
    for (indexType c = 0; c < columns; ++c) {
        if (std::isnan(matrixValue(data, rows, row, c))) {
            return true;
        }
    }
    return false;
}
//=============================================================================
static ArrayOf
rowArray(const double* data, indexType rows, indexType row, indexType columns)
{
    double* out = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, columns));
    for (indexType c = 0; c < columns; ++c) {
        out[c] = matrixValue(data, rows, row, c);
    }
    return ArrayOf(NLS_DOUBLE, Dimensions(1, columns), out);
}
//=============================================================================
static PDist2Options
parsePDist2Options(Evaluator* eval, const ArrayOfVector& args, const ArrayOf& x)
{
    PDist2Options opts;
    size_t pos = 2;
    indexType columns = x.columns();
    if (pos < args.size() && !isPDist2OptionName(args[pos])) {
        if (args[pos].isFunctionHandle()) {
            if (eval == nullptr) {
                Error("Nelson:statistics:evaluatorExpected", "Evaluator expected.");
            }
            function_handle fh = args[pos].asFunctionHandle();
            if (fh.anonymousHandle == nullptr) {
                Error("Nelson:statistics:invalidDistanceFunction", "Invalid distance function.");
            }
            opts.customDistance = true;
            opts.customFunction = reinterpret_cast<FunctionDef*>(fh.anonymousHandle);
        } else {
            opts.metric = distanceMetric(args[pos]);
        }
        ++pos;
    }
    if (!opts.customDistance) {
        if (opts.metric == DistanceMetric::Minkowski) {
            if (pos < args.size() && !isPDist2OptionName(args[pos])) {
                opts.minkowskiPower = scalarDouble(args[pos++],
                    Nelson::resolveMessageTemplate(L"Nelson:statistics:distanceParameterNumeric",
                        L"Distance parameter must be numeric."));
                if (!(opts.minkowskiPower > 0)) {
                    Error("Nelson:validators:mustBePositive", "%s must be positive.",
                        "Distance parameter");
                }
            }
        } else if (opts.metric == DistanceMetric::StandardizedEuclidean) {
            if (pos < args.size() && !isPDist2OptionName(args[pos])) {
                opts.scale = vectorParameter(args[pos++], columns,
                    Nelson::resolveMessageTemplate(L"Nelson:statistics:distanceParameterVector",
                        L"Distance parameter must be a vector."));
            }
        } else if (opts.metric == DistanceMetric::Mahalanobis) {
            if (pos < args.size() && !isPDist2OptionName(args[pos])) {
                opts.invCov = covarianceParameter(args[pos++], columns).inverse();
            }
        } else if (pos < args.size() && !isPDist2OptionName(args[pos])) {
            Error("Nelson:statistics:distanceParameterNotSupported",
                "Distance parameter is not supported for this metric.");
        }
    }
    if ((args.size() - pos) % 2 != 0) {
        Error("Nelson:statistics:nameValueNotPaired", "Name-value arguments must be paired.");
    }
    for (; pos < args.size(); pos += 2) {
        std::wstring name = pdist2OptionName(args[pos]);
        if (name == L"smallest" || name == L"largest") {
            if (opts.selectSmallest || opts.selectLargest) {
                Error("Nelson:statistics:smallestAndLargest",
                    "Smallest and Largest cannot both be specified.");
            }
            opts.selectCount = positiveIntegerScalar(args[pos + 1],
                formatI18nMessage(
                    Nelson::resolveMessageTemplate(L"Nelson:statistics:mustBePositiveInteger",
                        L"%s must be a positive integer scalar."),
                    L"K"));
            opts.selectSmallest = name == L"smallest";
            opts.selectLargest = name == L"largest";
        } else {
            positiveIntegerScalar(args[pos + 1],
                formatI18nMessage(
                    Nelson::resolveMessageTemplate(L"Nelson:statistics:mustBePositiveInteger",
                        L"%s must be a positive integer scalar."),
                    L"CacheSize"));
        }
    }
    return opts;
}
//=============================================================================
static double
pdist2BuiltInDistance(const double* x, indexType xRows, const double* y, indexType yRows,
    indexType columns, indexType xRow, indexType yRow, const PDist2Options& opts,
    const std::vector<bool>& xNaN, const std::vector<bool>& yNaN)
{
    if (xNaN[xRow] || yNaN[yRow]) {
        return std::numeric_limits<double>::quiet_NaN();
    }
    KNNOptions knnOpts;
    knnOpts.metric = opts.metric;
    knnOpts.minkowskiPower = opts.minkowskiPower;
    knnOpts.scale = opts.scale;
    knnOpts.invCov = opts.invCov;
    return opts.metric == DistanceMetric::Mahalanobis
        ? mahalanobisXY(x, xRows, y, yRows, columns, xRow, yRow, opts.invCov)
        : matrixDistance(x, xRows, y, yRows, columns, xRow, yRow, opts.metric, knnOpts);
}
//=============================================================================
static void
pdist2CustomRow(Evaluator* eval, const PDist2Options& opts, const double* xData, indexType xRows,
    indexType xRow, indexType columns, const ArrayOf& y, double* out)
{
    ArrayOfVector input;
    input << rowArray(xData, xRows, xRow, columns);
    input << y;
    ArrayOfVector result = opts.customFunction->evaluateFunction(eval, input, 1);
    if (result.empty()) {
        Error(
            "Nelson:statistics:invalidDistanceFunctionOutput:distanceFunctionReturnADistanceVector",
            "Distance function must return a distance vector.");
    }
    ArrayOf values = asDoubleMatrix(result[0],
        Nelson::resolveMessageTemplate(L"Nelson:statistics:distanceFunctionReturnReal",
            L"Distance function must return real numeric values."));
    if (!values.isVector() || values.elementCount() != y.rows()) {
        Error(
            "Nelson:statistics:invalidDistanceFunctionOutput:distanceFunctionReturnOneDistancePer",
            "Distance function must return one distance per row in Y.");
    }
    const double* ptr = static_cast<const double*>(values.dataPointer());
    for (indexType yRow = 0; yRow < y.rows(); ++yRow) {
        out[xRow + yRow * xRows] = ptr[yRow];
    }
}
//=============================================================================
static ArrayOf
pdist2FullMatrix(Evaluator* eval, const ArrayOf& x, const ArrayOf& y, const PDist2Options& opts)
{
    indexType xRows = x.rows();
    indexType yRows = y.rows();
    indexType columns = x.columns();
    double* out = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, xRows * yRows));
    const double* xData = static_cast<const double*>(x.dataPointer());
    const double* yData = static_cast<const double*>(y.dataPointer());
    if (opts.customDistance) {
        for (indexType xRow = 0; xRow < xRows; ++xRow) {
            pdist2CustomRow(eval, opts, xData, xRows, xRow, columns, y, out);
        }
        return ArrayOf(NLS_DOUBLE, Dimensions(xRows, yRows), out);
    }
    std::vector<bool> xNaN(xRows, false);
    std::vector<bool> yNaN(yRows, false);
    for (indexType r = 0; r < xRows; ++r) {
        xNaN[r] = rowHasNaN(xData, xRows, r, columns);
    }
    for (indexType r = 0; r < yRows; ++r) {
        yNaN[r] = rowHasNaN(yData, yRows, r, columns);
    }
    PDist2Options localOpts = opts;
    std::vector<double> xRanks;
    std::vector<double> yRanks;
    if (localOpts.metric == DistanceMetric::Spearman) {
        xRanks = rankRows(xData, xRows, columns);
        yRanks = rankRows(yData, yRows, columns);
        xData = xRanks.data();
        yData = yRanks.data();
        localOpts.metric = DistanceMetric::Correlation;
    }
#if WITH_OPENMP
#pragma omp parallel for if (xRows * yRows > OMP_DEFAULT_THRESHOLD)
#endif
    for (ompIndexType yy = 0; yy < (ompIndexType)yRows; ++yy) {
        indexType yRow = static_cast<indexType>(yy);
        for (indexType xRow = 0; xRow < xRows; ++xRow) {
            out[xRow + yRow * xRows] = pdist2BuiltInDistance(
                xData, xRows, yData, yRows, columns, xRow, yRow, localOpts, xNaN, yNaN);
        }
    }
    return ArrayOf(NLS_DOUBLE, Dimensions(xRows, yRows), out);
}
//=============================================================================
static bool
pdist2NeighborLess(const Neighbor& a, const Neighbor& b, bool largest)
{
    bool aNaN = std::isnan(a.distance);
    bool bNaN = std::isnan(b.distance);
    if (aNaN != bNaN) {
        return !aNaN;
    }
    if (a.distance == b.distance || (aNaN && bNaN)) {
        return a.index < b.index;
    }
    return largest ? a.distance > b.distance : a.distance < b.distance;
}
//=============================================================================
static ArrayOfVector
pdist2Selected(const ArrayOf& full, const PDist2Options& opts, int nLhs)
{
    const double* data = static_cast<const double*>(full.dataPointer());
    indexType xRows = full.rows();
    indexType yRows = full.columns();
    indexType k = std::min(opts.selectCount, xRows);
    double* distances = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, k * yRows));
    double* indices = nLhs > 1
        ? static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, k * yRows))
        : nullptr;
    for (indexType yRow = 0; yRow < yRows; ++yRow) {
        std::vector<Neighbor> values;
        values.reserve(xRows);
        for (indexType xRow = 0; xRow < xRows; ++xRow) {
            values.push_back({ xRow + 1, data[xRow + yRow * xRows] });
        }
        std::stable_sort(
            values.begin(), values.end(), [&opts](const Neighbor& a, const Neighbor& b) {
                return pdist2NeighborLess(a, b, opts.selectLargest);
            });
        for (indexType row = 0; row < k; ++row) {
            distances[row + yRow * k] = values[row].distance;
            if (indices) {
                indices[row + yRow * k] = static_cast<double>(values[row].index);
            }
        }
    }
    ArrayOfVector output;
    output << ArrayOf(NLS_DOUBLE, Dimensions(k, yRows), distances);
    if (indices) {
        output << ArrayOf(NLS_DOUBLE, Dimensions(k, yRows), indices);
    }
    return output;
}
//=============================================================================
ArrayOfVector
PDist2(Evaluator* eval, const ArrayOfVector& args, int nLhs)
{
    ArrayOf x = asDoubleMatrix(args[0],
        Nelson::resolveMessageTemplate(
            L"Nelson:statistics:xYNumericMatrices", L"X and Y must be numeric matrices."));
    ArrayOf y = asDoubleMatrix(args[1],
        Nelson::resolveMessageTemplate(
            L"Nelson:statistics:xYNumericMatrices", L"X and Y must be numeric matrices."));
    if (x.columns() != y.columns()) {
        Error(
            "Nelson:statistics:xyColumnMismatch", "X and Y must have the same number of columns.");
    }
    PDist2Options opts = parsePDist2Options(eval, args, x);
    if (opts.metric == DistanceMetric::StandardizedEuclidean && opts.scale.empty()) {
        opts.scale
            = defaultScale(static_cast<const double*>(x.dataPointer()), x.rows(), x.columns());
    }
    if (opts.metric == DistanceMetric::Mahalanobis && opts.invCov.size() == 0) {
        opts.invCov
            = defaultCovariance(static_cast<const double*>(x.dataPointer()), x.rows(), x.columns())
                  .inverse();
    }
    if (nLhs > 1 && !opts.selectSmallest && !opts.selectLargest) {
        Error("Nelson:statistics:smallestOrLargestRequired",
            "Smallest or Largest must be specified to return indices.");
    }
    ArrayOf full = pdist2FullMatrix(eval, x, y, opts);
    if (opts.selectSmallest || opts.selectLargest) {
        return pdist2Selected(full, opts, nLhs);
    }
    ArrayOfVector output;
    output << full;
    return output;
}
//=============================================================================
