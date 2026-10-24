//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include <cstddef>
#include <string>
#include <vector>
//=============================================================================
namespace Nelson::HistogramBinning {
//=============================================================================
// Pure bin-edge math for GOHistogram, kept apart from the property handling.
//=============================================================================
// Default (auto) BinLimits derived from the data: [min, max], widened by half a
// unit on each side when the data are constant so the property stays increasing.
std::vector<double>
defaultBinLimits(const std::vector<double>& data);
//=============================================================================
std::vector<double>
validateEdges(const std::vector<double>& values);
//=============================================================================
std::vector<double>
validateBinLimits(const std::vector<double>& values);
//=============================================================================
// True when every finite value is (within tolerance) an integer.
bool
allIntegers(const std::vector<double>& data);
//=============================================================================
// True when [xmin, xmax] is nearly constant, i.e. the data span collapses to a point.
bool
isNearlyConstant(double xmin, double xmax);
//=============================================================================
// nBins equal-width edges spanning exactly [low, high] (hard bin limits).
std::vector<double>
uniformEdges(double low, double high, std::size_t nBins);
//=============================================================================
// Edges on a grid of the given width, anchored on multiples of the width and
// extended to cover [low, high].
std::vector<double>
edgesFromWidth(double low, double high, double width);
//=============================================================================
// Integer rule: unit-width bins centred on integers, covering [xmin, xmax].
// Returns an empty vector when it would need more than 65536 bins.
std::vector<double>
integerRuleEdges(double xmin, double xmax);
//=============================================================================
// Nearly-constant rule: a single bin bracketing the constant value.
std::vector<double>
nearlyConstantEdges(double xmin, double xmax);
//=============================================================================
// Scott's rule bin width rounded to a 'nice' value (1/2/3/5/10 x 10^k).
// Returns 0 when it cannot be computed (empty or nearly-constant data).
double
scottNiceBinWidth(const std::vector<double>& data, double xmin, double xmax);
//=============================================================================
// Bivariate Scott's rule bin width (n^(1/4) sample-count exponent), rounded to a
// 'nice' value. Used per dimension by two-dimensional histograms.
double
scottNiceBinWidth2D(const std::vector<double>& data, double xmin, double xmax);
//=============================================================================
// Bivariate Freedman-Diaconis rule bin width (n^(1/4) exponent), rounded to a
// 'nice' value; falls back to Scott's rule when the interquartile range is zero.
double
fdNiceBinWidth2D(const std::vector<double>& data, double xmin, double xmax);
//=============================================================================
// Integer rule using rounded data extremes: unit-width bins centred on the
// integers nearest [xmin, xmax]. Returns empty when it would need > 65536 bins.
std::vector<double>
integerRuleEdgesRounded(double xmin, double xmax);
//=============================================================================
// Automatic edge selection for one dimension of a bivariate histogram, given the
// raw data for that dimension and the BinMethod ('auto', 'scott', 'fd' or
// 'integers'). Returns empty when the method has no automatic rule so the caller
// falls back to its own NumBins-based layout.
std::vector<double>
autoEdges2D(const std::vector<double>& data, const std::wstring& binMethod);
//=============================================================================
// Used when a bin count is requested with data-derived limits: a 'nice' grid that may
// extend past [xmin, xmax] with exactly nbins bins.
std::vector<double>
niceNumBinsEdges(double xmin, double xmax, std::size_t nbins);
//=============================================================================
} // namespace Nelson::HistogramBinning
//=============================================================================
