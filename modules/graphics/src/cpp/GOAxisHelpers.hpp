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
getTicksInner(double amin, double amax, bool isLog, int requestedCounts);
//=============================================================================
void
formatAxisManual(TEXT_INTERPRETER_FORMAT textFormat, double t1, double t2, int tickcount,
    bool isLogarithmic, double& tStart, double& tStop, std::vector<double>& tickLocations,
    wstringVector& tlabels);
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
minMaxVector(double* vals, int len, double& vmin, double& vmax);
//=============================================================================
void
rescale(double& amin, double& amax, double& ascale);
//=============================================================================
void
rerange(double& amin, double& amax, double arange);
//=============================================================================
};
//=============================================================================
