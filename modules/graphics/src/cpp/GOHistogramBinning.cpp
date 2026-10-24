//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "GOHistogramBinning.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include <algorithm>
#include <cmath>
#include <limits>
//=============================================================================
namespace Nelson::HistogramBinning {
//=============================================================================
namespace {
    //=============================================================================
    // Spacing to the next representable double above |value| (matches eps(x)).
    double
    epsOfMagnitude(double value)
    {
        const double a = std::fabs(value);
        return std::nextafter(a, std::numeric_limits<double>::infinity()) - a;
    }
    //=============================================================================
    // Threshold separating the normal branch from the nearly-constant branch.
    double
    constantThreshold(double xmin, double xmax)
    {
        const double xscale = std::max(std::fabs(xmin), std::fabs(xmax));
        return std::max(std::sqrt(std::numeric_limits<double>::epsilon()) * xscale,
            std::numeric_limits<double>::min());
    }
    //=============================================================================
    // Rounds a raw bin width to the nearest 'nice' value (1/2/3/5/10 x 10^k).
    // Returns 0 for a non-positive width.
    double
    niceBinWidth(double rawBinWidth)
    {
        if (!(rawBinWidth > 0.) || !std::isfinite(rawBinWidth)) {
            return 0.;
        }
        const double p = std::pow(10., std::floor(std::log10(rawBinWidth)));
        const double rel = rawBinWidth / p;
        if (rel < 1.5) {
            return 1. * p;
        }
        if (rel < 2.5) {
            return 2. * p;
        }
        if (rel < 4.) {
            return 3. * p;
        }
        if (rel < 7.5) {
            return 5. * p;
        }
        return 10. * p;
    }
    //=============================================================================
    // Sample standard deviation (n-1 denominator), 0 for fewer than two values.
    double
    sampleStdDev(const std::vector<double>& data)
    {
        const size_t n = data.size();
        if (n < 2) {
            return 0.;
        }
        double mean = 0.;
        for (double value : data) {
            mean += value;
        }
        mean /= static_cast<double>(n);
        double variance = 0.;
        for (double value : data) {
            const double delta = value - mean;
            variance += delta * delta;
        }
        return std::sqrt(variance / static_cast<double>(n - 1));
    }
    //=============================================================================
    // Interquartile range using linear interpolation between order statistics.
    double
    interquartileRange(std::vector<double> data)
    {
        const size_t n = data.size();
        if (n < 2) {
            return 0.;
        }
        std::sort(data.begin(), data.end());
        auto quantile = [&](double q) {
            const double pos = q * (static_cast<double>(n) - 1.);
            const double lo = std::floor(pos);
            const size_t idx = static_cast<size_t>(lo);
            const double frac = pos - lo;
            if (idx + 1 >= n) {
                return data[n - 1];
            }
            return data[idx] + frac * (data[idx + 1] - data[idx]);
        };
        return quantile(0.75) - quantile(0.25);
    }
    //=============================================================================
} // namespace
//=============================================================================
std::vector<double>
defaultBinLimits(const std::vector<double>& data)
{
    if (data.empty()) {
        return { 0., 1. };
    }
    auto [minIt, maxIt] = std::minmax_element(data.begin(), data.end());
    double low = *minIt;
    double high = *maxIt;
    if (low == high) {
        low -= 0.5;
        high += 0.5;
    }
    return { low, high };
}
//=============================================================================
std::vector<double>
validateEdges(const std::vector<double>& values)
{
    if (values.size() < 2) {
        Error("Nelson:histogram:binEdgesTooFew", "BinEdges must have at least two elements.");
    }
    for (size_t k = 1; k < values.size(); ++k) {
        if (!std::isfinite(values[k]) || values[k] < values[k - 1]) {
            Error("Nelson:histogram:invalidBinEdges",
                "BinEdges must be a nondecreasing finite vector.");
        }
    }
    return values;
}
//=============================================================================
std::vector<double>
validateBinLimits(const std::vector<double>& values)
{
    if (values.size() != 2 || !std::isfinite(values[0]) || !std::isfinite(values[1])
        || values[0] >= values[1]) {
        Error("Nelson:histogram:invalidBinLimits",
            "BinLimits must be a two-element increasing finite vector.");
    }
    return values;
}
//=============================================================================
bool
allIntegers(const std::vector<double>& data)
{
    if (data.empty()) {
        return false;
    }
    for (double value : data) {
        if (std::fabs(value - std::round(value)) > 1e-12) {
            return false;
        }
    }
    return true;
}
//=============================================================================
bool
isNearlyConstant(double xmin, double xmax)
{
    return (xmax - xmin) <= constantThreshold(xmin, xmax);
}
//=============================================================================
std::vector<double>
uniformEdges(double low, double high, std::size_t nBins)
{
    if (nBins < 1) {
        nBins = 1;
    }
    double width = (high - low) / static_cast<double>(nBins);
    if (!std::isfinite(width) || width <= 0.) {
        width = 1.;
    }
    std::vector<double> edges(nBins + 1);
    for (size_t k = 0; k <= nBins; ++k) {
        edges[k] = low + width * static_cast<double>(k);
    }
    edges[nBins] = high;
    return edges;
}
//=============================================================================
std::vector<double>
edgesFromWidth(double low, double high, double width)
{
    if (!std::isfinite(width) || width <= 0.) {
        Error("Nelson:histogram:invalidBinWidth", "BinWidth must be a positive finite scalar.");
    }
    low = std::floor(low / width) * width;
    high = std::ceil(high / width) * width;
    if (low == high) {
        high = low + width;
    }
    size_t nBins = static_cast<size_t>(std::ceil((high - low) / width));
    if (nBins < 1) {
        nBins = 1;
    }
    std::vector<double> edges(nBins + 1);
    for (size_t k = 0; k <= nBins; ++k) {
        edges[k] = low + width * static_cast<double>(k);
    }
    if (edges.back() < high) {
        edges.push_back(edges.back() + width);
    }
    return edges;
}
//=============================================================================
std::vector<double>
integerRuleEdges(double xmin, double xmax)
{
    double low = std::floor(xmin) - 0.5;
    double high = std::ceil(xmax) + 0.5;
    size_t nBins = static_cast<size_t>(std::max(1., std::round(high - low)));
    if (nBins > 65536) {
        return {};
    }
    std::vector<double> edges(nBins + 1);
    for (size_t k = 0; k <= nBins; ++k) {
        edges[k] = low + static_cast<double>(k);
    }
    return edges;
}
//=============================================================================
std::vector<double>
nearlyConstantEdges(double xmin, double xmax)
{
    const double xscale = std::max(std::fabs(xmin), std::fabs(xmax));
    const double binRange = std::max(1., std::ceil(epsOfMagnitude(xscale)));
    const double leftEdge = std::floor(2. * (xmin - binRange / 4.)) / 2.;
    const double rightEdge = std::ceil(2. * (xmax + binRange / 4.)) / 2.;
    return { leftEdge, rightEdge };
}
//=============================================================================
double
scottNiceBinWidth(const std::vector<double>& data, double xmin, double xmax)
{
    const size_t n = data.size();
    if (n < 1) {
        return 0.;
    }
    const double sd = sampleStdDev(data);
    const double xrange = xmax - xmin;
    const double rawBinWidth
        = (sd > 0.) ? 3.5 * sd / std::cbrt(static_cast<double>(n)) : xrange / 10.;
    return niceBinWidth(rawBinWidth);
}
//=============================================================================
double
scottNiceBinWidth2D(const std::vector<double>& data, double xmin, double xmax)
{
    const size_t n = data.size();
    if (n < 1) {
        return 0.;
    }
    const double sd = sampleStdDev(data);
    const double xrange = xmax - xmin;
    // Bivariate Scott's rule: the sample count enters as n^(1/4) instead of the univariate
    // n^(1/3), which gives coarser binning for two-dimensional histograms.
    const double rawBinWidth
        = (sd > 0.) ? 3.5 * sd / std::pow(static_cast<double>(n), 0.25) : xrange / 10.;
    return niceBinWidth(rawBinWidth);
}
//=============================================================================
double
fdNiceBinWidth2D(const std::vector<double>& data, double xmin, double xmax)
{
    const size_t n = data.size();
    if (n < 1) {
        return 0.;
    }
    const double iqr = interquartileRange(data);
    const double xrange = xmax - xmin;
    // Bivariate Freedman-Diaconis rule (n^(1/4) sample-count exponent), falling
    // back to Scott's rule when the interquartile range collapses to zero.
    const double rawBinWidth = (iqr > 0.) ? 2. * iqr / std::pow(static_cast<double>(n), 0.25) : 0.;
    if (rawBinWidth > 0.) {
        return niceBinWidth(rawBinWidth);
    }
    return scottNiceBinWidth2D(data, xmin, xmax);
}
//=============================================================================
std::vector<double>
integerRuleEdgesRounded(double xmin, double xmax)
{
    const double low = std::round(xmin) - 0.5;
    const double high = std::round(xmax) + 0.5;
    size_t nBins = static_cast<size_t>(std::max(1., std::round(high - low)));
    if (nBins > 65536) {
        return {};
    }
    std::vector<double> edges(nBins + 1);
    for (size_t k = 0; k <= nBins; ++k) {
        edges[k] = low + static_cast<double>(k);
    }
    return edges;
}
//=============================================================================
std::vector<double>
autoEdges2D(const std::vector<double>& data, const std::wstring& binMethod)
{
    std::vector<double> finite;
    finite.reserve(data.size());
    for (double value : data) {
        if (std::isfinite(value)) {
            finite.push_back(value);
        }
    }
    if (finite.empty()) {
        return {};
    }
    auto [minIt, maxIt] = std::minmax_element(finite.begin(), finite.end());
    const double xmin = *minIt;
    const double xmax = *maxIt;
    const double xrange = xmax - xmin;
    // Integer rule (unit bins centred on integers): always for the explicit
    // 'integers' method, and for 'auto' only when the data are integers spanning
    // a small range (threshold of 50). Applied per dimension.
    const bool integerAuto = (binMethod == L"auto") && allIntegers(finite) && xrange <= 50.;
    if (binMethod == L"integers" || integerAuto) {
        std::vector<double> edges = integerRuleEdgesRounded(xmin, xmax);
        if (!edges.empty()) {
            return edges;
        }
    }
    if (binMethod == L"auto" || binMethod == L"scott" || binMethod == L"fd") {
        if (isNearlyConstant(xmin, xmax)) {
            return nearlyConstantEdges(xmin, xmax);
        }
        const double width = (binMethod == L"fd") ? fdNiceBinWidth2D(finite, xmin, xmax)
                                                  : scottNiceBinWidth2D(finite, xmin, xmax);
        if (width > 0.) {
            return edgesFromWidth(xmin, xmax, width);
        }
    }
    return {};
}
//=============================================================================
std::vector<double>
niceNumBinsEdges(double xmin, double xmax, std::size_t nbins)
{
    if (nbins < 1) {
        nbins = 1;
    }
    const double n = static_cast<double>(nbins);
    const double xscale = std::max(std::fabs(xmin), std::fabs(xmax));
    const double xrange = xmax - xmin;
    const double rawBinWidth = std::max(xrange / n, epsOfMagnitude(xscale));
    double leftEdge = 0.;
    double rightEdge = 0.;
    double binWidth = 0.;
    if (xrange > constantThreshold(xmin, xmax)) {
        const double powOfTen = std::pow(10., std::floor(std::log10(rawBinWidth)));
        const double relSize = rawBinWidth / powOfTen;
        binWidth = powOfTen * std::floor(relSize);
        leftEdge = std::max(std::min(binWidth * std::floor(xmin / binWidth), xmin),
            -std::numeric_limits<double>::max());
        if (nbins > 1) {
            const double ll = (xmax - leftEdge) / n;
            const double ul = (xmax - leftEdge) / (n - 1.);
            const double p10 = std::pow(10., std::floor(std::log10(ul - ll)));
            binWidth = p10 * std::ceil(ll / p10);
        }
        rightEdge
            = std::min(std::max(leftEdge + n * binWidth, xmax), std::numeric_limits<double>::max());
    } else {
        const double binRange = std::max(1., std::ceil(n * epsOfMagnitude(xscale)));
        leftEdge = std::floor(2. * (xmin - binRange / 4.)) / 2.;
        rightEdge = std::ceil(2. * (xmax + binRange / 4.)) / 2.;
        binWidth = (rightEdge - leftEdge) / n;
    }
    std::vector<double> edges(nbins + 1);
    if (!std::isfinite(binWidth)) {
        for (size_t k = 0; k <= nbins; ++k) {
            edges[k] = leftEdge + (rightEdge - leftEdge) * (static_cast<double>(k) / n);
        }
    } else {
        edges[0] = leftEdge;
        for (size_t k = 1; k < nbins; ++k) {
            edges[k] = leftEdge + static_cast<double>(k) * binWidth;
        }
    }
    edges[nbins] = rightEdge;
    return edges;
}
//=============================================================================
} // namespace Nelson::HistogramBinning
//=============================================================================
