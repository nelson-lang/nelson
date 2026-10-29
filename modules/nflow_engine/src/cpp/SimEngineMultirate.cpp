//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "SimEngineMultirate.hpp"
#include "FieldNames.hpp"
#include "NFlowCodegenHelpers.hpp"
#include <algorithm>
#include <cmath>
#include <functional>
#include <nlohmann/json.hpp>
//=============================================================================
namespace Nelson::NFlow {
//=============================================================================
// Floating-point GCD (tolerance-based Euclid): the largest g of which a and b
// are near integer multiples. Used to refine the base step for non-integer
// sample times.
static double
sampleTimeGcd(double a, double b)
{
    a = std::abs(a);
    b = std::abs(b);
    const double tol = 1e-9 * (std::max)(a, b) + 1e-15;
    int guard = 100000;
    while (b > tol && guard-- > 0) {
        const double r = a - std::floor(a / b) * b;
        a = b;
        b = r;
    }
    return a;
}
//=============================================================================
void
refineBaseStepForSampleTimes(const std::vector<Block>& blocks, double& dt, int& samples)
{
    const double dtOld = dt;
    const int samplesOld = samples;
    double base = dt;
    bool refine = false;
    for (const Block& b : blocks) {
        if (!asJson(b.params).contains("SampleTime")
            || !asJson(b.params)["SampleTime"].is_number()) {
            continue;
        }
        const double ts = asJson(b.params)["SampleTime"].get<double>();
        if (ts <= dtOld * (1.0 + 1e-9)) {
            continue; // <= base, or inherited (-1): nothing to refine
        }
        const double ratio = ts / dtOld;
        const double rounded = std::floor(ratio + 0.5);
        if (std::abs(ratio - rounded) > 1e-6 * rounded) {
            base = sampleTimeGcd(base, ts);
            refine = true;
        }
    }
    if (refine && base > 1e-6) {
        const double factor = dtOld / base;
        if (factor <= 1000.0 + 1e-9) {
            dt = base;
            samples = static_cast<int>(
                std::floor(static_cast<double>(samplesOld) * dtOld / base + 0.5));
        }
    }
}
//=============================================================================
std::optional<std::string>
computeUpdateRates(const std::vector<Block>& blocks, double dt, const std::vector<int>& updateIdx,
    const std::vector<int>& algIdx, std::vector<int>& updateRate, const std::vector<double>& time,
    const std::vector<BlockState>& blockState, const std::vector<std::vector<int>>& inputIds)
{
    for (size_t i = 0; i < blocks.size(); ++i) {
        const auto& p = asJson(blocks[i].params);
        if (p.contains("SampleTime") && p["SampleTime"].is_number()) {
            const double ts = p["SampleTime"].get<double>();
            if (ts > dt * (1.0 + 1e-9)) {
                const double ratio = ts / dt;
                const double rounded = std::floor(ratio + 0.5);
                // Require an integer sub-rate: a non-integer multiple would be
                // snapped to the nearest integer, and near a half-integer the
                // snap depends on floating point.
                if (std::abs(ratio - rounded) > 1e-6 * rounded) {
                    return buildOutputJson("error",
                        "block '" + blocks[i].id + "' SampleTime " + doubleToStr(ts)
                            + " is not an integer multiple of the base step " + doubleToStr(dt)
                            + " (multi-rate requires integer sub-rates)",
                        time, blocks, blockState, inputIds, "");
                }
                // zoh/ddelay/foh/dtf/dstateSpace gate their UPDATE on their own
                // 'SampleTime' (BlockState::dNextTime); an updateRate on top would
                // double-schedule them.
                const std::string& bt = blocks[i].type;
                if (bt == "zoh" || bt == "ddelay" || bt == "foh" || bt == "dtf"
                    || bt == "dstateSpace") {
                    continue;
                }
                // A sub-rate block must hold its output between hits: a state-holding
                // block (UPDATE phase) re-emits its state, a feedthrough block in the
                // algebraic schedule uses BlockState::heldOutput.
                const bool isStateHolding
                    = std::find(updateIdx.begin(), updateIdx.end(), (int)i) != updateIdx.end();
                const bool isAlgebraic
                    = std::find(algIdx.begin(), algIdx.end(), (int)i) != algIdx.end();
                if (!isStateHolding && !isAlgebraic) {
                    return buildOutputJson("error",
                        "block '" + blocks[i].id
                            + "' has a SampleTime but no state to hold between samples; "
                              "multi-rate is supported on state-holding discrete blocks "
                              "(e.g. unitDelay), feedthrough math blocks, or via a "
                              "rateTransition block",
                        time, blocks, blockState, inputIds, "");
                }
                updateRate[i] = (std::max)(1, (int)rounded);
            }
        }
    }

    // updateRate only covers top-level blocks, so a SampleTime nested inside a
    // subsystem would be ignored: reject it (recursing into nested subsystems)
    // instead of a silent no-op.
    {
        std::function<std::string(const nlohmann::json&)> firstNestedRate
            = [&](const nlohmann::json& innerBlocks) -> std::string {
            for (const auto& ib : innerBlocks) {
                const nlohmann::json ip = (ib.contains("params") && ib["params"].is_object())
                    ? ib["params"]
                    : nlohmann::json::object();
                // A self-scheduled block gates its own advance on its own clock, so
                // a slower rate nested here needs nothing from the engine schedule.
                if (ip.contains("SampleTime") && ip["SampleTime"].is_number()
                    && ip["SampleTime"].get<double>() > dt * (1.0 + 1e-9)
                    && !nflow::blockSelfSchedulesRate(ib.value(nflow::kType, std::string()))) {
                    return ib.value("id", std::string());
                }
                if (ip.contains(nflow::kSubsystem) && ip[nflow::kSubsystem].is_object()
                    && ip[nflow::kSubsystem].contains(nflow::kBlocks)
                    && ip[nflow::kSubsystem][nflow::kBlocks].is_array()) {
                    std::string deeper = firstNestedRate(ip[nflow::kSubsystem][nflow::kBlocks]);
                    if (!deeper.empty()) {
                        return deeper;
                    }
                }
            }
            return std::string();
        };
        for (const Block& b : blocks) {
            if (asJson(b.params).contains(nflow::kSubsystem)
                && asJson(b.params)[nflow::kSubsystem].is_object()
                && asJson(b.params)[nflow::kSubsystem].contains(nflow::kBlocks)
                && asJson(b.params)[nflow::kSubsystem][nflow::kBlocks].is_array()) {
                std::string nestedId
                    = firstNestedRate(asJson(b.params)[nflow::kSubsystem][nflow::kBlocks]);
                if (!nestedId.empty()) {
                    return buildOutputJson("error",
                        "block '" + nestedId
                            + "' has a SampleTime but is nested inside a subsystem; multi-rate "
                              "is only supported on top-level blocks",
                        time, blocks, blockState, inputIds, "");
                }
            }
        }
    }
    return std::nullopt;
}
//=============================================================================
} // namespace Nelson::NFlow
//=============================================================================
