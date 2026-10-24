//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include <string>
#include <list>
#include <vector>
#include "Types.hpp"
#include "GOTextInterpreterProperty.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
double
tickLog(double x);
//=============================================================================
std::list<double>
getTicksInner(double amin, double amax, bool isLog, int requestedCounts, int maxIntervals = 10);
//=============================================================================
void
formatAxisManual(TEXT_INTERPRETER_FORMAT textFormat, double t1, double t2, int tickcount,
    bool isLogarithmic, double& tStart, double& tStop, std::vector<double>& tickLocations,
    wstringVector& tlabels);
//=============================================================================
void
formatAxisTickLabels(TEXT_INTERPRETER_FORMAT textFormat, const std::vector<double>& tickLocations,
    bool isLogarithmic, wstringVector& tickLabels);
//=============================================================================
int
tickLabelCommonExponent(const std::vector<double>& tickLocations, bool isLogarithmic);
//=============================================================================
std::list<double>
getTicksOuter(double amin, double amax, bool isLog, int requestedCounts);
//=============================================================================
void
formatAxisAuto(TEXT_INTERPRETER_FORMAT textFormat, double tMin, double tMax, int tickcount,
    bool isLogarithmic, double& tStart, double& tStop, std::vector<double>& tickLocations,
    wstringVector& tlabels);
//=============================================================================
void
formatAxisAuto(TEXT_INTERPRETER_FORMAT textFormat, double tMin, double tMax, int tickcount,
    bool isLogarithmic, const std::wstring& limitMethod, double& tStart, double& tStop,
    std::vector<double>& tickLocations, wstringVector& tlabels);
//=============================================================================
std::vector<double>
getMinorTicks(
    const std::vector<double>& tickLocations, double limMin, double limMax, bool isLogarithmic);
//=============================================================================
// Expected automatic ticks for one axis, computed from the final
// axis limits (raw values, not log10) and the axis length in pixels.
// labelAngleDeg is the screen angle of the axis direction (0 = horizontal,
// 90 = vertical); label clearance follows min(gapX/|cos|, gapY/|sin|).
// forcedMaxCount > 0 overrides the pixel-based tick budget (degenerate axes).
std::vector<double>
computeReferenceAutoTicks(double lo, double hi, double axisLengthPx, double labelAngleDeg,
    bool isLog, int forcedMaxCount, double fontSizePoints = 10.0);
//=============================================================================
// Outward tick-aligned exponent limits for an automatic log axis whose data
// spans [dataMin, dataMax] (linear, > 0). Shares the decade-stride density
// model of computeReferenceAutoTicks so the limit and the ticks regenerated
// from it land on one grid. Returns false when the data extent is not a valid
// positive interval.
bool
computeLogAxisAlignedLimits(double dataMin, double dataMax, double axisLengthPx,
    double labelAngleDeg, double fontSizePoints, double& loExponent, double& hiExponent);
//=============================================================================
void
minMaxVector(double* vals, int len, double& vmin, double& vmax);
//=============================================================================
void
rescale(double& amin, double& amax, double& ascale);
//=============================================================================
void
rerange(double& amin, double& amax, double arange);
//=============================================================================
} // namespace Nelson
//=============================================================================
