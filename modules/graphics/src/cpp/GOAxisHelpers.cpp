//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <fmt/printf.h>
#include <fmt/format.h>
#include <fmt/xchar.h>
#include <cmath>
#include "GOAxisHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
std::wstring
trimTickPrint(double val, bool scientificNotation)
{
    std::wstring buffer;
    if (scientificNotation) {
        std::wstring label = fmt::sprintf(L"%e", val);
        size_t ePtr = label.size() - 1;
        while ((label[ePtr] != L'e') && (label[ePtr] != L'E')) {
            ePtr--;
        }
        ePtr--;
        while (label[ePtr] == L'0') {
            label.erase(ePtr, 1);
            ePtr--;
        }
        if ((label[ePtr] == L'.') || (label[ePtr] == L',')) {
            label.insert(ePtr + 1, L"0");
        }
        ePtr = label.size() - 1;
        while ((label[ePtr] != L'e') && (label[ePtr] != L'E')) {
            ePtr--;
        }
        ePtr += 2;
        while (ePtr < (unsigned int)(label.size()) && (label[ePtr] == L'0')) {
            label.erase(ePtr, 1);
        }
        if (ePtr == (unsigned int)(label.size())) {
            label.append(L"0");
        }
        return label;
    } else {
        buffer = fmt::sprintf(L"%f", val);
        while (buffer.back() == L'0') {
            buffer.pop_back();
        }
        if ((buffer.back() == L'.') || (buffer.back() == L',')) {
            buffer.pop_back();
        }
        return buffer;
    }
}
//=============================================================================
double
tickLog(double x)
{
    if (x > 0) {
        return log10(x);
    }
    return -10;
}
//=============================================================================
std::list<double>
getTicksInner(double amin, double amax, bool isLog, int requestedCounts)
{
    double arange = amax - amin;
    double astep = pow(10.0, floor(log10(arange)));
    double nsteps = arange / astep;
    double aval;
    std::list<double> retvec;
    if (!std::isfinite(amin) || !std::isfinite(amax)) {
        return retvec;
    }
    if (requestedCounts >= 10) {
        if (nsteps <= 1) {
            astep /= 10.0;
        } else if (nsteps <= 2) {
            astep /= 5.0;
        } else if (nsteps <= 5) {
            astep /= 2.0;
        }
    } else if (requestedCounts >= 5) {
        if (nsteps > 5) {
            astep *= 2.0;
            nsteps = arange / astep;
        }
        if (nsteps <= 1) {
            astep /= 5.0;
        }
    } else if (requestedCounts >= 2) {
        if (nsteps > 2) {
            astep *= 5.0;
            nsteps = arange / astep;
        }
        if (nsteps <= 1) {
            astep /= 2.0;
        }
    }
    if (isLog) {
        astep = ceil(astep);
    }
    if ((amin < 0) && (amax > 0)) {
        aval = 0;
        while (aval <= amax) {
            retvec.push_back(aval);
            aval += astep;
        }
        aval = -astep;
        while (aval >= amin) {
            retvec.push_front(aval);
            aval -= astep;
        }
    } else if ((amin == 0) && (amax > 0)) {
        aval = 0;
        while (aval <= amax) {
            retvec.push_back(aval);
            aval += astep;
        }
    } else if ((amin < 0) && (amax == 0)) {
        aval = 0;
        while (aval >= amin) {
            retvec.push_front(aval);
            aval -= astep;
        }
    } else {
        aval = ceil(amin / astep) * astep;
        while (aval <= amax) {
            retvec.push_back(aval);
            aval += astep;
        }
    }
    return retvec;
}
//=============================================================================
void
formatAxisManual(double t1, double t2, int tickcount, bool isLogarithmic, double& tStart,
    double& tStop, std::vector<double>& tickLocations, wstringVector& tickLabels)
{
    tickLocations.clear();
    tickLabels.clear();
    bool exponentialForm = false;
    std::list<double> tick_locations(getTicksInner(t1, t2, isLogarithmic, tickcount));
    if (tick_locations.empty()) {
        return;
    }
    tStart = tick_locations.front();
    tStop = tick_locations.back();
    size_t tCount = tick_locations.size();
    for (size_t i = 0; i < tCount; i++) {
        auto l_front = tick_locations.begin();
        std::advance(l_front, i);
        double tloc = *l_front;
        if (!isLogarithmic) {
            tickLocations.push_back(tloc);
        } else {
            tickLocations.push_back(pow(10.0, tloc));
        }
        if (tloc != 0.0) {
            exponentialForm |= (fabs(log10(fabs(tloc))) >= 4.0);
        }
    }
    for (size_t i = 0; i < tCount; i++) {
        auto l_front = tick_locations.begin();
        std::advance(l_front, i);
        double tloc = *l_front;
        if (!isLogarithmic) {
            tickLabels.push_back(trimTickPrint(tloc, exponentialForm));
        } else {
            tickLabels.push_back(trimTickPrint(pow(10.0, tloc), true));
        }
    }
}
//=============================================================================
std::list<double>
getTicksOuter(double amin, double amax, bool isLog, int requestedCounts)
{
    double arange = amax - amin;
    double astep = pow(10.0, floor(log10(arange)));
    double nsteps = arange / astep;
    double aval;
    std::list<double> retvec;
    if (!std::isfinite(amin) || !std::isfinite(amax)) {
        return retvec;
    }
    if (requestedCounts >= 10) {
        if (nsteps <= 1) {
            astep /= 10.0;
        } else if (nsteps <= 2) {
            astep /= 5.0;
        } else if (nsteps <= 5) {
            astep /= 2.0;
        }
    } else if (requestedCounts >= 5) {
        if (nsteps > 5) {
            astep *= 2.0;
            nsteps = arange / astep;
        }
        if (nsteps <= 1) {
            astep /= 5.0;
        }
    } else if (requestedCounts >= 2) {
        if (nsteps > 2) {
            astep *= 5.0;
            nsteps = arange / astep;
        }
        if (nsteps <= 1) {
            astep /= 2.0;
        }
    }
    if (isLog) {
        astep = ceil(astep);
    }
    if ((amin < 0) && (amax > 0)) {
        aval = 0;
        while (aval < amax) {
            retvec.push_back(aval);
            aval += astep;
        }
        retvec.push_back(aval);
        aval = -astep;
        while (aval > amin) {
            retvec.push_front(aval);
            aval -= astep;
        }
        retvec.push_front(aval);
    } else if ((amin == 0) && (amax > 0)) {
        aval = 0;
        while (aval < amax) {
            retvec.push_back(aval);
            aval += astep;
        }
        retvec.push_back(aval);
    } else if ((amin < 0) && (amax == 0)) {
        aval = 0;
        while (aval > amin) {
            retvec.push_front(aval);
            aval -= astep;
        }
        retvec.push_front(aval);
    } else {
        aval = floor(amin / astep) * astep;
        while (aval < amax) {
            retvec.push_back(aval);
            aval += astep;
        }
        retvec.push_back(aval);
    }
    return retvec;
}
//=============================================================================
void
formatAxisAuto(double tMin, double tMax, int tickcount, bool isLogarithmic, double& tStart,
    double& tStop, std::vector<double>& tickLocations, wstringVector& tlabels)
{
    tickLocations.clear();
    tlabels.clear();
    bool exponentialForm = false;
    std::list<double> tick_locations(getTicksOuter(tMin, tMax, isLogarithmic, tickcount));
    if (tick_locations.empty()) {
        return;
    }
    tStart = tick_locations.front();
    tStop = tick_locations.back();
    size_t tCount = tick_locations.size();
    for (size_t i = 0; i < tCount; i++) {
        auto l_front = tick_locations.begin();
        std::advance(l_front, i);
        double tloc = *l_front;
        if (!isLogarithmic) {
            tickLocations.push_back(tloc);
        } else {
            tickLocations.push_back(pow(10.0, tloc));
        }
        if (tloc != 0.0) {
            exponentialForm |= (fabs(log10(fabs(tloc))) >= 4.0);
        }
    }
    for (size_t i = 0; i < tCount; i++) {
        auto l_front = tick_locations.begin();
        std::advance(l_front, i);
        double tloc = *l_front;
        if (!isLogarithmic) {
            tlabels.push_back(trimTickPrint(tloc, exponentialForm));
        } else {
            tlabels.push_back(trimTickPrint(pow(10.0, tloc), true));
        }
    }
}
//=============================================================================
void
minMaxVector(double* vals, int len, double& vmin, double& vmax)
{
    vmin = vals[0];
    vmax = vals[0];
    for (int i = 0; i < len; i++) {
        vmin = std::min(vals[i], vmin);
        vmax = std::max(vals[i], vmax);
    }
}
//=============================================================================
void
rescale(double& amin, double& amax, double& ascale)
{
    double amean = (amin + amax) / 2.0;
    amin = amean - ascale * (amean - amin);
    amax = amean + ascale * (amax - amean);
}
//=============================================================================
void
rerange(double& amin, double& amax, double arange)
{
    double amean = (amin + amax) / 2.0;
    amin = amean - arange / 2.0;
    amax = amean + arange / 2.0;
}
//=============================================================================
};
//=============================================================================
