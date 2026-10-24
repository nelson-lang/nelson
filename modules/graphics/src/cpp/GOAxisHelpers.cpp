//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#define FMT_HEADER_ONLY
#define _USE_MATH_DEFINES
#include <fmt/format.h>
#include <fmt/xchar.h>
#include <algorithm>
#include <cmath>
#include <limits>
#include <sstream>
#include <iomanip>
#include "GOAxisHelpers.hpp"
#include "GOPropertyValues.hpp"
#include "StringHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static wstringVector
split(const std::wstring& str, const std::wstring& token)
{
    wstringVector result;
    std::wstring wstr(str);
    while (wstr.size()) {
        size_t index = wstr.find(token);
        if (index != std::wstring::npos) {
            result.push_back(wstr.substr(0, index));
            wstr = wstr.substr(index + token.size());
            if (str.size() == 0) {
                result.push_back(wstr);
            }
        } else {
            result.push_back(wstr);
            wstr = L"";
        }
    }
    return result;
}
//=============================================================================
static std::wstring
stripTrailingZeros(const std::wstring& input)
{
    size_t dotPos = input.find_last_of(L'.');
    if (dotPos == std::wstring::npos) {
        return input;
    }
    size_t lastNonZero = input.find_last_not_of(L'0');
    size_t length = (lastNonZero == dotPos) ? lastNonZero : lastNonZero + 1;
    return input.substr(0, length);
}
//=============================================================================
static std::wstring
formatTick(TEXT_INTERPRETER_FORMAT textFormat, double val, bool scientificNotation)
{
    if (val == 0.0) {
        return L"0";
    }
    if (scientificNotation) {
        std::wstring label = fmt::format(L"{:e}", val);
        if (textFormat == TEXT_INTERPRETER_FORMAT::TEX_MARKUP) {
            wstringVector elements = split(label, L"e");
            if (elements.size() == 2) {
                std::wstring& mantissa = elements[0];
                std::wstring& exponent = elements[1];
                mantissa = stripTrailingZeros(mantissa);
                wchar_t sign = L'+';
                if (!exponent.empty() && (exponent[0] == L'+' || exponent[0] == L'-')) {
                    sign = exponent[0];
                    exponent = exponent.substr(1);
                }
                if (exponent.empty()) {
                    exponent = L"0";
                }

                int expValue = std::stoi(exponent);
                exponent = std::to_wstring(expValue);

                if (sign == L'-' && expValue != 0) {
                    exponent = L"-" + exponent;
                }

                std::wostringstream labelBuilder;
                if (exponent == L"0") {
                    labelBuilder << L"10^{0}";
                } else {
                    if (mantissa == L"1") {
                        labelBuilder << L"10^{" << exponent << L"}";
                    } else {
                        labelBuilder << mantissa << L"×10^{" << exponent << L"}";
                    }
                }
                label = labelBuilder.str();
            }
        } else {
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
        }
        return label;
    } else {
        std::wstring buffer = fmt::format(L"{:f}", val);
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
int
tickLabelCommonExponent(const std::vector<double>& tickLocations, bool isLogarithmic)
{
    if (isLogarithmic || tickLocations.empty()) {
        return 0;
    }
    double maxAbs = 0.0;
    for (double tickLocation : tickLocations) {
        if (!std::isfinite(tickLocation)) {
            return 0;
        }
        maxAbs = std::max(maxAbs, std::fabs(tickLocation));
    }
    if (maxAbs == 0.0) {
        return 0;
    }
    int exponent = static_cast<int>(std::floor(std::log10(maxAbs)));
    // When the largest tick is an exact power of ten, one power less is factored
    // out: a 1e6..1e7 axis labels 1e6 as "1" (factor 1e6), not "0.1" (factor 1e7).
    // This holds on the large and small side, with or without a zero tick.
    bool exactPower = std::fabs(maxAbs - std::pow(10.0, exponent)) <= 1e-9 * maxAbs;
    if (exactPower) {
        exponent -= 1;
    }
    return (exponent >= 4 || exponent <= -3) ? exponent : 0;
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
static bool
tickLessEqual(double value, double limit, double step)
{
    return value <= limit + std::fabs(step) * 1e-10;
}
//=============================================================================
static bool
tickGreaterEqual(double value, double limit, double step)
{
    return value >= limit - std::fabs(step) * 1e-10;
}
//=============================================================================
static double
niceTickStep(double amin, double amax, int requestedCounts, int maxIntervals = 10)
{
    double range = std::fabs(amax - amin);
    if (!std::isfinite(range) || range <= 0) {
        return 1.0;
    }
    int targetIntervals = std::max(1, std::min(requestedCounts - 1, maxIntervals));
    double rawStep = range / static_cast<double>(targetIntervals);
    if (!std::isfinite(rawStep) || rawStep <= 0) {
        return 1.0;
    }
    double magnitude = std::pow(10.0, std::floor(std::log10(rawStep)));
    double normalized = rawStep / magnitude;
    static const double SQRT_2 = std::sqrt(2.0);
    static const double SQRT_10 = std::sqrt(10.0);
    if (normalized < SQRT_2) {
        return magnitude;
    }
    if (normalized < SQRT_10) {
        return 2.0 * magnitude;
    }
    if (normalized <= 5.8) {
        return 5.0 * magnitude;
    }
    return 10.0 * magnitude;
}
//=============================================================================
static bool
isAlignedWithStep(double value, double step)
{
    if (!std::isfinite(value) || !std::isfinite(step) || step <= 0) {
        return false;
    }
    double ratio = value / step;
    return std::fabs(ratio - std::round(ratio)) <= 1e-10 * std::max(1.0, std::fabs(ratio));
}
//=============================================================================
static int
tickIntervalCount(double amin, double amax, double step)
{
    double first = std::ceil(amin / step) * step;
    double last = std::floor(amax / step) * step;
    if (last < first) {
        return 0;
    }
    return static_cast<int>(std::floor((last - first) / step + 0.5)) + 1;
}
//=============================================================================
static double
niceManualTickStep(double amin, double amax, int requestedCounts, int maxIntervals)
{
    (void)requestedCounts;
    double step = niceTickStep(amin, amax, maxIntervals + 1, maxIntervals);
    if (!std::isfinite(step) || step <= 0) {
        return 1.0;
    }
    int maxTickCount = std::max(2, maxIntervals + 1);
    if (isAlignedWithStep(amin, step) && isAlignedWithStep(amax, step)
        && tickIntervalCount(amin, amax, step) <= maxTickCount) {
        return step;
    }
    double magnitude = std::pow(10.0, std::floor(std::log10(step)));
    const double multipliers[] = { 1.0, 2.0, 5.0, 10.0 };
    for (int decade = 0; decade < 8; ++decade) {
        double scale = magnitude * std::pow(10.0, decade);
        for (double multiplier : multipliers) {
            double candidate = multiplier * scale;
            if (candidate < step) {
                continue;
            }
            if (isAlignedWithStep(amin, candidate) && isAlignedWithStep(amax, candidate)
                && tickIntervalCount(amin, amax, candidate) <= maxTickCount) {
                return candidate;
            }
        }
    }
    return step;
}
//=============================================================================
static std::list<double>
buildTicks(double amin, double amax, double astep, bool includeOuter)
{
    std::list<double> retvec;
    if (!std::isfinite(amin) || !std::isfinite(amax) || !std::isfinite(astep) || astep <= 0) {
        return retvec;
    }
    if (amax < amin) {
        std::swap(amin, amax);
    }
    double first = 0;
    double last = 0;
    if (includeOuter) {
        double lowerMin = std::floor(amin / astep) * astep;
        double upperMin = std::ceil(amin / astep) * astep;
        double lowerMax = std::floor(amax / astep) * astep;
        double upperMax = std::ceil(amax / astep) * astep;
        double snapTolerance = std::max(std::fabs(astep) * 1e-5, 1e-12);
        first = (std::fabs(amin - upperMin) <= snapTolerance) ? upperMin : lowerMin;
        last = (std::fabs(amax - lowerMax) <= snapTolerance) ? lowerMax : upperMax;
    } else {
        double lower = std::floor(amin / astep) * astep;
        double upper = std::ceil(amax / astep) * astep;
        double snapTolerance = std::max(std::fabs(astep) * 1e-5, 1e-12);
        first
            = (std::fabs(amin - lower) <= snapTolerance) ? lower : std::ceil(amin / astep) * astep;
        last
            = (std::fabs(amax - upper) <= snapTolerance) ? upper : std::floor(amax / astep) * astep;
    }
    if (last < first) {
        return retvec;
    }
    int guard = 0;
    for (double value = first; tickLessEqual(value, last, astep) && guard < 10000;
        value += astep, ++guard) {
        if (std::fabs(value) <= std::fabs(astep) * 1e-12) {
            value = 0;
        }
        retvec.push_back(value);
    }
    return retvec;
}
//=============================================================================
std::list<double>
getTicksInner(double amin, double amax, bool isLog, int requestedCounts, int maxIntervals)
{
    if (!std::isfinite(amin) || !std::isfinite(amax)) {
        return {};
    }
    if (amax < amin) {
        std::swap(amin, amax);
    }
    double astep = isLog ? 1.0 : niceManualTickStep(amin, amax, requestedCounts, maxIntervals);
    if (isLog && requestedCounts > 1 && std::fabs(amax - amin) > requestedCounts) {
        astep = std::ceil(std::fabs(amax - amin) / static_cast<double>(requestedCounts - 1));
    }
    return buildTicks(amin, amax, astep, false);
}
//=============================================================================
void
formatAxisManual(TEXT_INTERPRETER_FORMAT textFormat, double t1, double t2, int tickcount,
    bool isLogarithmic, double& tStart, double& tStop, std::vector<double>& tickLocations,
    wstringVector& tickLabels)
{
    tickLocations.clear();
    tickLabels.clear();
    bool exponentialForm = false;
    std::list<double> tick_locations(getTicksInner(t1, t2, isLogarithmic, tickcount, 10));
    if (tick_locations.empty()) {
        return;
    }
    tStart = tick_locations.front();
    tStop = tick_locations.back();
    size_t tCount = tick_locations.size();
    // A tick that is a negligible fraction of the axis span counts as zero in the
    // exponential-form test, so a near-zero residue does not force scientific
    // notation.
    double nearZeroTol = (!isLogarithmic && std::isfinite(tStop - tStart) && (tStop - tStart) > 0.0)
        ? 1e-9 * (tStop - tStart)
        : 0.0;
    for (size_t i = 0; i < tCount; i++) {
        auto l_front = tick_locations.begin();
        std::advance(l_front, i);
        double tloc = *l_front;
        if (!isLogarithmic) {
            tickLocations.push_back(tloc);
        } else {
            tickLocations.push_back(pow(10.0, tloc));
        }
        if (tloc != 0.0 && fabs(tloc) > nearZeroTol) {
            exponentialForm |= (fabs(log10(fabs(tloc))) >= 4.0);
        }
    }
    for (size_t i = 0; i < tCount; i++) {
        auto l_front = tick_locations.begin();
        std::advance(l_front, i);
        double tloc = *l_front;
        if (!isLogarithmic) {
            tickLabels.push_back(formatTick(textFormat, tloc, exponentialForm));
        } else {
            tickLabels.push_back(formatTick(textFormat, pow(10.0, tloc), true));
        }
    }
}
//=============================================================================
void
formatAxisTickLabels(TEXT_INTERPRETER_FORMAT textFormat, const std::vector<double>& tickLocations,
    bool isLogarithmic, wstringVector& tickLabels)
{
    tickLabels.clear();
    if (isLogarithmic) {
        // Sub-decade log axes carry linear tick values labelled plainly.
        bool decadeTicks = !tickLocations.empty();
        for (double tickLocation : tickLocations) {
            double d = std::log10(tickLocation);
            if (!std::isfinite(d) || std::fabs(d - std::round(d)) > 1e-9) {
                decadeTicks = false;
                break;
            }
        }
        isLogarithmic = decadeTicks;
    }
    int commonExponent = tickLabelCommonExponent(tickLocations, isLogarithmic);
    double commonScale = commonExponent != 0 ? std::pow(10.0, commonExponent) : 1.0;
    if (commonExponent != 0) {
        for (double tickLocation : tickLocations) {
            tickLabels.push_back(formatTick(textFormat, tickLocation / commonScale, false));
        }
        return;
    }
    bool exponentialForm = false;
    if (!isLogarithmic) {
        // A tick that is a negligible fraction of the axis span counts as zero in the
        // exponential-form test, so a near-zero residue does not force scientific
        // notation.
        double tickMin = std::numeric_limits<double>::infinity();
        double tickMax = -std::numeric_limits<double>::infinity();
        for (double tickLocation : tickLocations) {
            tickMin = std::min(tickMin, tickLocation);
            tickMax = std::max(tickMax, tickLocation);
        }
        double nearZeroTol = (std::isfinite(tickMax - tickMin) && (tickMax - tickMin) > 0.0)
            ? 1e-9 * (tickMax - tickMin)
            : 0.0;
        for (double tickLocation : tickLocations) {
            if (tickLocation != 0.0 && fabs(tickLocation) > nearZeroTol) {
                exponentialForm |= (fabs(log10(fabs(tickLocation))) >= 4.0);
            }
        }
    }
    for (double tickLocation : tickLocations) {
        tickLabels.push_back(
            formatTick(textFormat, tickLocation, isLogarithmic || exponentialForm));
    }
}
//=============================================================================
// Chooses a "nice" tick step of 1/2/5 x 10^k for tick-aligned automatic limits.
// Keeps the steps whose outward-snapped limits [floor(amin/step)*step,
// ceil(amax/step)*step] span at most maxIntervals intervals, then picks the one
// that adds the least padding beyond the data. Ties go to the finest step, so a
// data maximum already on a step multiple keeps the limit tight.
static double
niceStep(double amin, double amax, int maxIntervals)
{
    double range = std::fabs(amax - amin);
    if (!std::isfinite(range) || range <= 0) {
        return 1.0;
    }
    if (maxIntervals < 1) {
        maxIntervals = 1;
    }
    double rawStep = range / static_cast<double>(maxIntervals);
    if (!std::isfinite(rawStep) || rawStep <= 0) {
        return 1.0;
    }
    int loDecade = static_cast<int>(std::floor(std::log10(rawStep))) - 1;
    int hiDecade = static_cast<int>(std::ceil(std::log10(range))) + 1;
    static const double multipliers[] = { 1.0, 2.0, 5.0 };
    double bestStep = 0.0;
    double bestPadding = std::numeric_limits<double>::infinity();
    const double paddingTol = 1e-9 * std::max(1.0, range);
    for (int decade = loDecade; decade <= hiDecade; ++decade) {
        double scale = std::pow(10.0, decade);
        for (double multiplier : multipliers) {
            double step = multiplier * scale;
            if (!std::isfinite(step) || step <= 0) {
                continue;
            }
            double lo = std::floor(amin / step) * step;
            double hi = std::ceil(amax / step) * step;
            double intervals = (hi - lo) / step;
            long intervalCount = std::lround(intervals);
            if (intervalCount < 1 || intervalCount > maxIntervals) {
                continue;
            }
            double padding = (amin - lo) + (hi - amax);
            // Steps are in ascending order: replace only on strictly smaller padding, so
            // the first minimal one (the finest) wins.
            if (padding < bestPadding - paddingTol) {
                bestPadding = padding;
                bestStep = step;
            }
        }
    }
    if (bestStep <= 0.0) {
        return niceTickStep(amin, amax, maxIntervals + 1, maxIntervals);
    }
    return bestStep;
}
//=============================================================================
std::list<double>
getTicksOuter(double amin, double amax, bool isLog, int requestedCounts)
{
    if (!std::isfinite(amin) || !std::isfinite(amax)) {
        return {};
    }
    if (amax < amin) {
        std::swap(amin, amax);
    }
    // Tick-aligned automatic limits use an interval budget from the requested tick
    // count, capped at 10 intervals / 11 ticks. The count is used directly (not
    // count - 1), so N ticks may land on the slightly denser step that keeps the
    // limit tight.
    int maxIntervals = std::min(10, std::max(1, requestedCounts));
    double astep = isLog ? 1.0 : niceStep(amin, amax, maxIntervals);
    if (isLog && requestedCounts > 1 && std::fabs(amax - amin) > requestedCounts) {
        astep = std::ceil(std::fabs(amax - amin) / static_cast<double>(requestedCounts - 1));
    }
    return buildTicks(amin, amax, astep, true);
}
//=============================================================================
void
formatAxisAuto(TEXT_INTERPRETER_FORMAT textFormat, double tMin, double tMax, int tickcount,
    bool isLogarithmic, double& tStart, double& tStop, std::vector<double>& tickLocations,
    wstringVector& tlabels)
{
    formatAxisAuto(textFormat, tMin, tMax, tickcount, isLogarithmic,
        GO_PROPERTY_VALUE_TICKALIGNED_STR, tStart, tStop, tickLocations, tlabels);
}
//=============================================================================
void
formatAxisAuto(TEXT_INTERPRETER_FORMAT textFormat, double tMin, double tMax, int tickcount,
    bool isLogarithmic, const std::wstring& limitMethod, double& tStart, double& tStop,
    std::vector<double>& tickLocations, wstringVector& tlabels)
{
    tickLocations.clear();
    tlabels.clear();
    if (!std::isfinite(tMin) || !std::isfinite(tMax)) {
        return;
    }
    if (tMax < tMin) {
        std::swap(tMin, tMax);
    }
    if (tMax == tMin) {
        double delta = std::fabs(tMax) > 1 ? std::fabs(tMax) * 0.1 : 1.0;
        tMin -= delta;
        tMax += delta;
    }
    std::wstring method = limitMethod;
    if (method != GO_PROPERTY_VALUE_TIGHT_STR && method != GO_PROPERTY_VALUE_PADDED_STR) {
        method = GO_PROPERTY_VALUE_TICKALIGNED_STR;
    }
    if (method == GO_PROPERTY_VALUE_PADDED_STR) {
        double padding = (tMax - tMin) * 0.07;
        tMin -= padding;
        tMax += padding;
    }
    bool exponentialForm = false;
    int maxIntervals = std::max(1, tickcount - 1);
    std::list<double> tick_locations = method == GO_PROPERTY_VALUE_TICKALIGNED_STR
        ? getTicksOuter(tMin, tMax, isLogarithmic, tickcount)
        : getTicksInner(tMin, tMax, isLogarithmic, tickcount, maxIntervals);
    if (tick_locations.empty()) {
        tStart = tMin;
        tStop = tMax;
        return;
    }
    if (method == GO_PROPERTY_VALUE_TICKALIGNED_STR) {
        tStart = tick_locations.front();
        tStop = tick_locations.back();
    } else {
        tStart = tMin;
        tStop = tMax;
    }
    size_t tCount = tick_locations.size();
    // A tick that is a negligible fraction of the axis span counts as zero in the
    // exponential-form test, so a near-zero residue does not force scientific
    // notation.
    double nearZeroTol = (!isLogarithmic && std::isfinite(tStop - tStart) && (tStop - tStart) > 0.0)
        ? 1e-9 * (tStop - tStart)
        : 0.0;
    for (size_t i = 0; i < tCount; i++) {
        auto l_front = tick_locations.begin();
        std::advance(l_front, i);
        double tloc = *l_front;
        if (!isLogarithmic) {
            tickLocations.push_back(tloc);
        } else {
            tickLocations.push_back(pow(10.0, tloc));
        }
        if (tloc != 0.0 && fabs(tloc) > nearZeroTol) {
            exponentialForm |= (fabs(log10(fabs(tloc))) >= 4.0);
        }
    }
    for (size_t i = 0; i < tCount; i++) {
        auto l_front = tick_locations.begin();
        std::advance(l_front, i);
        double tloc = *l_front;
        if (!isLogarithmic) {
            tlabels.push_back(formatTick(textFormat, tloc, exponentialForm));
        } else {
            tlabels.push_back(formatTick(textFormat, pow(10.0, tloc), true));
        }
    }
}
//=============================================================================
std::vector<double>
getMinorTicks(
    const std::vector<double>& tickLocations, double limMin, double limMax, bool isLogarithmic)
{
    std::vector<double> minorTicks;
    if (tickLocations.size() < 2) {
        return minorTicks;
    }
    if (limMax < limMin) {
        std::swap(limMin, limMax);
    }
    constexpr size_t MAX_MINOR_TICKS = 1000;
    int subdivisions = isLogarithmic ? 8 : 4;
    for (size_t i = 0; i + 1 < tickLocations.size() && minorTicks.size() < MAX_MINOR_TICKS; ++i) {
        double t1 = tickLocations[i];
        double t2 = tickLocations[i + 1];
        if (!std::isfinite(t1) || !std::isfinite(t2) || t2 <= t1) {
            continue;
        }
        if (isLogarithmic && t1 > 0) {
            // The decade span between major ticks decides the minor subdivision. One
            // decade apart, the minors are the 2..9 multiples. When the stride skips
            // decades, the minors go on every intervening integer decade: 2..9 multiples
            // there would crowd into the first decade above each major and paint a solid
            // block of marks.
            double e1 = std::log10(t1);
            double e2 = std::log10(t2);
            long long d1 = std::llround(e1);
            long long d2 = std::llround(e2);
            if (d2 - d1 > 1) {
                for (long long d = d1 + 1; d < d2 && minorTicks.size() < MAX_MINOR_TICKS; ++d) {
                    double value = std::pow(10.0, static_cast<double>(d));
                    if (tickGreaterEqual(value, limMin, t2 - t1)
                        && tickLessEqual(value, limMax, t2 - t1)) {
                        minorTicks.push_back(value);
                    }
                }
            } else {
                for (int n = 2; n <= 9 && minorTicks.size() < MAX_MINOR_TICKS; ++n) {
                    double value = t1 * n;
                    if (value >= t2) {
                        break;
                    }
                    if (tickGreaterEqual(value, limMin, t2 - t1)
                        && tickLessEqual(value, limMax, t2 - t1)) {
                        minorTicks.push_back(value);
                    }
                }
            }
        } else {
            double step = (t2 - t1) / static_cast<double>(subdivisions + 1);
            for (int n = 1; n <= subdivisions && minorTicks.size() < MAX_MINOR_TICKS; ++n) {
                double value = t1 + step * n;
                if (tickGreaterEqual(value, limMin, step) && tickLessEqual(value, limMax, step)) {
                    minorTicks.push_back(value);
                }
            }
        }
    }
    return minorTicks;
}
//=============================================================================
void
minMaxVector(double* vals, int len, double& vmin, double& vmax)
{
    vmin = std::nan("");
    vmax = std::nan("");
    if (vals && len > 0) {
        vmin = vals[0];
        vmax = vals[0];
        for (int i = 0; i < len; i++) {
            vmin = std::min(vals[i], vmin);
            vmax = std::max(vals[i], vmax);
        }
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
// Automatic tick generation.
// Density: maxCount = min(11, floor(px/gap)+1); gap 45 px for horizontal-label
// axes (X), 28 px for stacked-label axes (Y). Preference order: smallest
// {1,2,5}x10^k step aligned to both limits that fits; both-aligned step
// exceeding by exactly one tick (trailing snapped tick dropped); smallest
// lower-aligned step; smallest step, both subject to a coverage rule
// (span/step - intervals <= 1.4). Values accumulate from the first tick so
// floating-point results are reproducible.
//=============================================================================
// Gaps scale with the axes font size (calibrated at the 10 pt default).
// Along an axis whose screen direction makes angle theta with the horizontal,
// labels do not collide when EITHER the horizontal or the vertical clearance
// is satisfied: gap(theta) = min(gapX/|cos|, gapY/|sin|).
static constexpr double REF_TICK_GAP_HORIZONTAL_PER_PT = 4.35;
static constexpr double REF_TICK_GAP_STACKED_PER_PT = 2.58;
static constexpr int REF_TICK_HARD_MAX = 11;
static constexpr double REF_TICK_ALIGN_TOL = 1e-9;
//=============================================================================
static double
refTickGap(double labelAngleDeg, double fontSizePoints)
{
    double theta = labelAngleDeg * M_PI / 180.0;
    double c = std::fabs(std::cos(theta));
    double s = std::fabs(std::sin(theta));
    double gapX = REF_TICK_GAP_HORIZONTAL_PER_PT * fontSizePoints;
    double gapY = REF_TICK_GAP_STACKED_PER_PT * fontSizePoints;
    double byWidth = c > 1e-9 ? gapX / c : std::numeric_limits<double>::infinity();
    double byHeight = s > 1e-9 ? gapY / s : std::numeric_limits<double>::infinity();
    return std::min(byWidth, byHeight);
}
//=============================================================================
struct RefTickCandidate
{
    double step = 0;
    long long k0 = 0;
    long long k1 = 0;
    bool dropLast = false;
    bool valid = false;
};
//=============================================================================
static bool
refAligned(double product, double limit, double step)
{
    return std::fabs(product - limit) <= REF_TICK_ALIGN_TOL * std::max(std::fabs(limit), step);
}
//=============================================================================
static std::vector<double>
refGenerateTicks(double lo, double hi, const RefTickCandidate& c)
{
    long long n = c.k1 - c.k0 + 1 - (c.dropLast ? 1 : 0);
    double first = static_cast<double>(c.k0) * c.step;
    bool loAligned = refAligned(first, lo, c.step);
    if (loAligned) {
        first = lo;
    }
    std::vector<double> ticks;
    ticks.reserve(static_cast<size_t>(n));
    ticks.push_back(first);
    double delta = 0.0;
    for (long long i = 1; i < n; ++i) {
        delta += c.step;
        ticks.push_back(first + delta);
    }
    if (!c.dropLast && refAligned(static_cast<double>(c.k1) * c.step, hi, c.step)) {
        ticks.back() = hi;
    }
    return ticks;
}
//=============================================================================
static std::vector<double>
refLinearAutoTicks(double lo, double hi, double axisLengthPx, double labelAngleDeg, int forcedMax,
    double fontSizePoints)
{
    double span = hi - lo;
    if (!std::isfinite(span) || span <= 0) {
        return {};
    }
    if (!(fontSizePoints > 0)) {
        fontSizePoints = 10.0;
    }
    int maxCount;
    if (forcedMax > 0) {
        maxCount = forcedMax;
    } else {
        double gap = refTickGap(labelAngleDeg, fontSizePoints);
        maxCount = std::max(
            2, std::min(REF_TICK_HARD_MAX, static_cast<int>(std::floor(axisLengthPx / gap)) + 1));
    }
    RefTickCandidate bothFit, bothOver, bothTwo, loFit, anyFit, anyLoose;
    int kStart = static_cast<int>(std::floor(std::log10(span))) - 3;
    static const double mantissas[3] = { 1.0, 2.0, 5.0 };
    for (int kk = kStart; kk < kStart + 8; ++kk) {
        for (double m : mantissas) {
            double s = m * std::pow(10.0, kk);
            double q0 = lo / s;
            double q1 = hi / s;
            if (std::fabs(q0) > 4e15 || std::fabs(q1) > 4e15) {
                continue;
            }
            long long k0 = static_cast<long long>(
                std::ceil(q0 - REF_TICK_ALIGN_TOL * std::max(1.0, std::fabs(q0))));
            long long k1 = static_cast<long long>(
                std::floor(q1 + REF_TICK_ALIGN_TOL * std::max(1.0, std::fabs(q1))));
            long long n = k1 - k0 + 1;
            if (n < 2) {
                continue;
            }
            bool loAl = refAligned(static_cast<double>(k0) * s, lo, s);
            bool hiAl = refAligned(static_cast<double>(k1) * s, hi, s);
            double marginLo = static_cast<double>(k0) * s - lo;
            double marginHi = hi - static_cast<double>(k1) * s;
            bool marginOk = n > 5 || std::max(marginLo, marginHi) <= 0.7 * s + 1e-12 * s;
            RefTickCandidate cand;
            cand.step = s;
            cand.k0 = k0;
            cand.k1 = k1;
            cand.valid = true;
            if (loAl && hiAl && n >= 3 && n <= maxCount && !bothFit.valid) {
                bothFit = cand;
            }
            if (loAl && hiAl && n == 2 && !bothTwo.valid) {
                bothTwo = cand;
            }
            if (loAl && hiAl && n == maxCount + 1 && !bothOver.valid) {
                double first = loAl ? lo : static_cast<double>(k0) * s;
                double delta = 0.0;
                for (long long i = 1; i < n; ++i) {
                    delta += s;
                }
                cand.dropLast = ((first + delta) != hi);
                bothOver = cand;
                cand.dropLast = false;
            }
            if (loAl && n <= maxCount && !loFit.valid) {
                loFit = cand;
            }
            if (n <= maxCount && marginOk && !anyFit.valid) {
                anyFit = cand;
            }
            if (n <= maxCount && !anyLoose.valid) {
                anyLoose = cand;
            }
        }
    }
    // A two-tick both-aligned candidate beats a saturated lower-aligned one
    // (n == maxCount): only clearly fitting lower-aligned candidates win.
    if (bothTwo.valid && loFit.valid && (loFit.k1 - loFit.k0 + 1) == maxCount) {
        loFit.valid = false;
    }
    for (const RefTickCandidate* c :
        { &bothFit, &bothOver, &loFit, &bothTwo, &anyFit, &anyLoose }) {
        if (c->valid) {
            return refGenerateTicks(lo, hi, *c);
        }
    }
    return {};
}
//=============================================================================
static std::vector<double>
refLogAutoTicks(
    double lo, double hi, double axisLengthPx, double labelAngleDeg, double fontSizePoints)
{
    if (!(lo > 0) || !(hi > lo) || !std::isfinite(lo) || !std::isfinite(hi)) {
        return {};
    }
    if (hi / lo < 10.0 * (1.0 - 1e-12)) {
        return refLinearAutoTicks(lo, hi, axisLengthPx, labelAngleDeg, 0, fontSizePoints);
    }
    double llo = std::log10(lo);
    double lhi = std::log10(hi);
    long long dlo = static_cast<long long>(std::ceil(llo - 1e-9));
    long long dhi = static_cast<long long>(std::floor(lhi + 1e-9));
    if (dhi < dlo) {
        return refLinearAutoTicks(lo, hi, axisLengthPx, labelAngleDeg, 0, fontSizePoints);
    }
    if (dhi == dlo) {
        return { std::pow(10.0, static_cast<double>(dlo)) };
    }
    bool loAl = std::fabs(llo - std::round(llo)) <= 1e-9;
    bool hiAl = std::fabs(lhi - std::round(lhi)) <= 1e-9;
    // Decade tick density follows the pixel/font gap model of the linear path: at
    // most REF_TICK_HARD_MAX (11) decades are shown at stride 1, then the stride
    // thins to {2,5,10,...} (1..1e10 -> 11 ticks, 1..1e12 -> every second decade).
    double gap = refTickGap(labelAngleDeg, fontSizePoints > 0 ? fontSizePoints : 10.0);
    int maxCount = std::max(
        2, std::min(REF_TICK_HARD_MAX, static_cast<int>(std::floor(axisLengthPx / gap)) + 1));
    long long bestA0 = 0;
    long long bestStride = 0;
    long long coverA0 = 0;
    long long coverStride = 0;
    static const long long strideMantissas[3] = { 1, 2, 5 };
    for (long long scale = 1; scale <= 10000000 && coverStride == 0; scale *= 10) {
        for (long long m : strideMantissas) {
            long long s = m * scale;
            long long a0 = ((dlo >= 0) ? ((dlo + s - 1) / s) : -((-dlo) / s)) * s;
            long long a1 = ((dhi >= 0) ? (dhi / s) : -(((-dhi) + s - 1) / s)) * s;
            if (a1 < a0) {
                continue;
            }
            long long n = (a1 - a0) / s + 1;
            if (n < 2 || n > maxCount) {
                continue;
            }
            // A stride that keeps both aligned decade limits among its ticks is
            // preferred (coverStride) over one that merely fits (bestStride).
            bool inclLo = loAl && a0 == dlo;
            bool inclHi = hiAl && a1 == dhi;
            if (bestStride == 0) {
                bestStride = s;
                bestA0 = a0;
            }
            if (inclLo && inclHi && coverStride == 0) {
                coverStride = s;
                coverA0 = a0;
            }
        }
    }
    long long stride = (coverStride != 0) ? coverStride : bestStride;
    long long anchor = (coverStride != 0) ? coverA0 : bestA0;
    if (stride == 0) {
        return { std::pow(10.0, static_cast<double>(dlo)) };
    }
    std::vector<double> ticks;
    for (long long d = anchor; d <= dhi; d += stride) {
        ticks.push_back(std::pow(10.0, static_cast<double>(d)));
    }
    return ticks;
}
//=============================================================================
static long long
floorToMultiple(long long v, long long s)
{
    return (v >= 0) ? (v / s) * s : -(((-v) + s - 1) / s) * s;
}
//=============================================================================
static long long
ceilToMultiple(long long v, long long s)
{
    return (v >= 0) ? ((v + s - 1) / s) * s : -((-v) / s) * s;
}
//=============================================================================
// Outward tick-aligned exponent limits for an automatic log axis whose data
// spans [dataMin, dataMax] (linear, > 0). The enclosing integer decades are
// snapped outward to a {1,2,5}x10^k decade stride chosen from the same
// pixel/font gap model as computeReferenceAutoTicks, so the limit and the ticks
// regenerated from it share one decade grid. A data maximum already on a decade
// keeps the limit tight; only a fractional excess pushes it to the next aligned
// decade.
bool
computeLogAxisAlignedLimits(double dataMin, double dataMax, double axisLengthPx,
    double labelAngleDeg, double fontSizePoints, double& loExponent, double& hiExponent)
{
    if (!(dataMin > 0) || !(dataMax > dataMin) || !std::isfinite(dataMin)
        || !std::isfinite(dataMax)) {
        return false;
    }
    double lmin = std::log10(dataMin);
    double lmax = std::log10(dataMax);
    long long eLo = static_cast<long long>(std::floor(lmin + 1e-9));
    long long eHi = static_cast<long long>(std::ceil(lmax - 1e-9));
    if (eHi <= eLo) {
        eHi = eLo + 1;
    }
    double gap = refTickGap(labelAngleDeg, fontSizePoints > 0 ? fontSizePoints : 10.0);
    int maxCount = std::max(
        2, std::min(REF_TICK_HARD_MAX, static_cast<int>(std::floor(axisLengthPx / gap)) + 1));
    static const long long strideMantissas[3] = { 1, 2, 5 };
    for (long long scale = 1; scale <= 10000000; scale *= 10) {
        for (long long m : strideMantissas) {
            long long s = m * scale;
            long long a0 = floorToMultiple(eLo, s);
            long long a1 = ceilToMultiple(eHi, s);
            long long n = (a1 - a0) / s + 1;
            if (n >= 2 && n <= maxCount) {
                loExponent = static_cast<double>(a0);
                hiExponent = static_cast<double>(a1);
                return true;
            }
        }
    }
    loExponent = static_cast<double>(eLo);
    hiExponent = static_cast<double>(eHi);
    return true;
}
//=============================================================================
std::vector<double>
computeReferenceAutoTicks(double lo, double hi, double axisLengthPx, double labelAngleDeg,
    bool isLog, int forcedMaxCount, double fontSizePoints)
{
    if (isLog) {
        return refLogAutoTicks(lo, hi, axisLengthPx, labelAngleDeg, fontSizePoints);
    }
    return refLinearAutoTicks(lo, hi, axisLengthPx, labelAngleDeg, forcedMaxCount, fontSizePoints);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
