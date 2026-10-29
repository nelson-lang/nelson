//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Simulation-progress scheduler: wall-clock publishing and incremental
// (delta) progress serialization. The invariants under test:
//   - the scheduler is period-driven and drift-free (unit-testable, no clock);
//   - concatenating every delta reconstructs the final series exactly, with no
//     overlap and no gap;
//   - numbers are locale-independent; NaN/inf follow the final-result policy;
//   - display carries only its latest value; toWorkspace never appears.
//=============================================================================
#include "test_helpers.hpp"
#include "NFlowCodegenHelpers.hpp"
#include "ProgressPublishScheduler.hpp"
#include <clocale>
#include <cmath>
#include <limits>
#include <string>
#include <vector>
//=============================================================================
using Nelson::NFlow::Block;
using Nelson::NFlow::BlockState;
using Nelson::NFlow::buildOutputJson;
using Nelson::NFlow::buildProgressJson;
using Nelson::NFlow::json;
using Nelson::NFlow::ProgressDeltaSession;
using Nelson::NFlow::ProgressPublishScheduler;
//=============================================================================
namespace {
//=============================================================================
constexpr long long MS = 1000000LL; // ns per ms
Block
makeBlock(const std::string& id, const std::string& type, int nid, int inputs)
{
    Block b;
    b.id = id;
    b.type = type;
    b.nid = nid;
    b.inputs = inputs;
    return b;
}
//=============================================================================
struct NumericLocaleGuard
{
    std::string previous;

    NumericLocaleGuard()
    {
        const char* current = std::setlocale(LC_NUMERIC, nullptr);
        if (current) {
            previous = current;
        }
    }

    ~NumericLocaleGuard()
    {
        if (!previous.empty()) {
            std::setlocale(LC_NUMERIC, previous.c_str());
        }
    }
};
//=============================================================================
} // namespace
//=============================================================================
TEST_CASE("progress scheduler: nothing due before the first period elapses")
{
    ProgressPublishScheduler sched(100 * MS);
    sched.start(0);
    CHECK_FALSE(sched.due(0));
    CHECK_FALSE(sched.due(99 * MS));
    CHECK(sched.due(100 * MS));
    CHECK(sched.due(250 * MS));
}
//=============================================================================
TEST_CASE("progress scheduler: unstarted scheduler is never due")
{
    ProgressPublishScheduler sched(100 * MS);
    CHECK_FALSE(sched.due(0));
    CHECK_FALSE(sched.due(1000 * MS));
}
//=============================================================================
TEST_CASE("progress scheduler: period measured from the previous publish (no burst)")
{
    ProgressPublishScheduler sched(100 * MS);
    sched.start(0);
    // A long stall (e.g. pause or a slow step) yields ONE due publish, not a
    // burst of catch-up frames.
    CHECK(sched.due(730 * MS));
    sched.markPublished(730 * MS);
    CHECK_FALSE(sched.due(800 * MS));
    CHECK(sched.due(830 * MS));
}
//=============================================================================
TEST_CASE("progress scheduler: simulated 10 Hz cadence over one second")
{
    ProgressPublishScheduler sched(100 * MS);
    sched.start(0);
    int published = 0;
    // Simulate a loop that checks every 7 ms of wall time for one second.
    for (long long now = 0; now <= 1000 * MS; now += 7 * MS) {
        if (sched.due(now)) {
            sched.markPublished(now);
            ++published;
        }
    }
    // ~10 publishes in one second; tolerance for the 7 ms check grid.
    CHECK(published >= 8);
    CHECK(published <= 11);
}
//=============================================================================
TEST_CASE("progress delta: concatenated deltas reconstruct the final series exactly")
{
    // A scope with two series over 10 samples, published in uneven windows.
    std::vector<Block> blocks { makeBlock("scope1", "scope", 0, 2) };
    std::vector<BlockState> states(1);
    std::vector<double> time;
    states[0].scopeSeries.assign(2, {});
    for (int k = 0; k < 10; ++k) {
        time.push_back(0.1 * k);
        states[0].scopeSeries[0].push_back(std::sin(0.1 * k));
        states[0].scopeSeries[1].push_back(std::cos(0.1 * k));
    }
    const std::vector<std::vector<int>> inputIds { { 0, 1 } };

    // Windows: [0,3) [3,4) [4,4) (empty) [4,10); no overlap, no gap.
    const int cuts[] = { 0, 3, 4, 4, 10 };
    std::vector<double> gotTimes;
    std::vector<double> gotS0;
    std::vector<double> gotS1;
    long long sequence = 0;
    for (int w = 0; w + 1 < (int)(sizeof(cuts) / sizeof(cuts[0])); ++w) {
        const std::string payload = buildProgressJson(sequence++, 0.5, time.back(), cuts[w],
            cuts[w + 1], time, blocks, states, inputIds, nullptr);
        const json parsed = json::parse(payload);
        CHECK(parsed["version"] == 2);
        CHECK(parsed["fromSample"] == cuts[w]);
        CHECK(parsed["toSample"] == cuts[w + 1]);
        const auto& sc = parsed["scopes"][0];
        CHECK(sc["type"] == "scope");
        for (const auto& v : sc["timesDelta"]) {
            gotTimes.push_back(v.get<double>());
        }
        for (const auto& v : sc["seriesDelta"][0]) {
            gotS0.push_back(v.get<double>());
        }
        for (const auto& v : sc["seriesDelta"][1]) {
            gotS1.push_back(v.get<double>());
        }
    }
    REQUIRE(gotTimes.size() == time.size());
    REQUIRE(gotS0.size() == states[0].scopeSeries[0].size());
    REQUIRE(gotS1.size() == states[0].scopeSeries[1].size());
    for (std::size_t k = 0; k < time.size(); ++k) {
        CHECK(gotTimes[k] == doctest::Approx(time[k]));
        CHECK(gotS0[k] == doctest::Approx(states[0].scopeSeries[0][k]));
        CHECK(gotS1[k] == doctest::Approx(states[0].scopeSeries[1][k]));
    }

    const json finalParsed
        = json::parse(buildOutputJson("done", "", time, blocks, states, inputIds, ""));
    const auto& finalScope = finalParsed["scopes"][0];
    for (std::size_t k = 0; k < time.size(); ++k) {
        CHECK(gotTimes[k] == doctest::Approx(finalScope["times"][k].get<double>()));
        CHECK(gotS0[k] == doctest::Approx(finalScope["series"][0][k].get<double>()));
        CHECK(gotS1[k] == doctest::Approx(finalScope["series"][1][k].get<double>()));
    }
}
//=============================================================================
TEST_CASE("progress delta: empty window is a progress-only frame")
{
    std::vector<Block> blocks { makeBlock("scope1", "scope", 0, 1) };
    std::vector<BlockState> states(1);
    states[0].scopeSeries = { { 1.0, 2.0 } };
    const std::vector<double> time { 0.0, 0.1 };
    const std::vector<std::vector<int>> inputIds { { 0 } };

    const json parsed = json::parse(
        buildProgressJson(7, 0.25, 0.1, 2, 2, time, blocks, states, inputIds, nullptr));
    CHECK(parsed["sequence"] == 7);
    CHECK(parsed["progress"] == doctest::Approx(0.25));
    CHECK(parsed["currentTime"] == doctest::Approx(0.1));
    CHECK(parsed["scopes"][0]["timesDelta"].empty());
    CHECK(parsed["scopes"][0]["seriesDelta"][0].empty());
}
//=============================================================================
TEST_CASE("progress delta: xy and xyz scopes carry parallel delta arrays")
{
    std::vector<Block> blocks { makeBlock("xy", "xyScope", 0, 2),
        makeBlock("xyz", "xyzScope", 1, 3) };
    std::vector<BlockState> states(2);
    for (int k = 0; k < 5; ++k) {
        states[0].xSeries.push_back(1.0 * k);
        states[0].ySeries.push_back(2.0 * k);
        states[1].xSeries.push_back(3.0 * k);
        states[1].ySeries.push_back(4.0 * k);
        states[1].zSeries.push_back(5.0 * k);
    }
    const std::vector<double> time { 0.0, 0.1, 0.2, 0.3, 0.4 };
    const std::vector<std::vector<int>> inputIds { { 0, 1 }, { 0, 1, 2 } };

    const json parsed = json::parse(
        buildProgressJson(1, 0.8, 0.4, 2, 5, time, blocks, states, inputIds, nullptr));
    const auto& xy = parsed["scopes"][0];
    CHECK(xy["type"] == "xyScope");
    REQUIRE(xy["xDelta"].size() == 3);
    REQUIRE(xy["yDelta"].size() == 3);
    CHECK(xy["xDelta"][0] == doctest::Approx(2.0));
    CHECK(xy["yDelta"][2] == doctest::Approx(8.0));
    const auto& xyz = parsed["scopes"][1];
    CHECK(xyz["type"] == "xyzScope");
    REQUIRE(xyz["zDelta"].size() == 3);
    CHECK(xyz["zDelta"][0] == doctest::Approx(10.0));
    CHECK(xyz["connected"].size() == 3);
}
//=============================================================================
TEST_CASE("progress delta: display carries only its latest value; toWorkspace is absent")
{
    std::vector<Block> blocks { makeBlock("disp", "display", 0, 1),
        makeBlock("tw", "toWorkspace", 1, 1) };
    std::vector<BlockState> states(2);
    states[0].scalar = 42.5;
    states[1].scopeSeries = { { 1.0, 2.0, 3.0 } };
    states[1].xSeries = { 0.0, 0.1, 0.2 };
    const std::vector<double> time { 0.0, 0.1, 0.2 };
    const std::vector<std::vector<int>> inputIds { { 0 }, { 1 } };

    const json parsed = json::parse(
        buildProgressJson(3, 1.0, 0.2, 0, 3, time, blocks, states, inputIds, nullptr));
    REQUIRE(parsed["scopes"].size() == 1);
    CHECK(parsed["scopes"][0]["type"] == "display");
    CHECK(parsed["scopes"][0]["value"] == doctest::Approx(42.5));
}
//=============================================================================
TEST_CASE("progress delta: numbers ignore the process numeric locale")
{
    NumericLocaleGuard localeGuard;
    const char* candidates[] = { "fr_FR.UTF-8", "fr_FR.utf8", "French_France.1252" };
    for (const char* candidate : candidates) {
        if (std::setlocale(LC_NUMERIC, candidate)) {
            break;
        }
    }

    std::vector<Block> blocks { makeBlock("scope1", "scope", 0, 1) };
    std::vector<BlockState> states(1);
    states[0].scopeSeries = { { 0.005, 0.25 } };
    const std::vector<double> time { 0.005, 0.05 };
    const std::vector<std::vector<int>> inputIds { { 0 } };

    const std::string payload
        = buildProgressJson(1, 0.005, 0.05, 0, 2, time, blocks, states, inputIds, nullptr);
    CHECK(payload.find(',') != std::string::npos); // separators exist...
    // ...but every number parses back: a decimal comma would break this.
    json parsed;
    bool parsedOk = true;
    try {
        parsed = json::parse(payload);
    } catch (...) {
        parsedOk = false;
    }
    CHECK(parsedOk);
    CHECK(parsed["progress"] == doctest::Approx(0.005));
    CHECK(parsed["scopes"][0]["timesDelta"][0] == doctest::Approx(0.005));
}
//=============================================================================
TEST_CASE("progress delta: NaN and infinities follow the final-result policy")
{
    std::vector<Block> blocks { makeBlock("scope1", "scope", 0, 1) };
    std::vector<BlockState> states(1);
    const double nan = std::numeric_limits<double>::quiet_NaN();
    const double inf = std::numeric_limits<double>::infinity();
    states[0].scopeSeries = { { nan, inf, -inf } };
    const std::vector<double> time { 0.0, 0.1, 0.2 };
    const std::vector<std::vector<int>> inputIds { { 0 } };

    const json parsed = json::parse(
        buildProgressJson(1, 0.5, 0.2, 0, 3, time, blocks, states, inputIds, nullptr));
    const auto& serie = parsed["scopes"][0]["seriesDelta"][0];
    CHECK(serie[0].is_null());
    CHECK(serie[1] == doctest::Approx(1e308));
    CHECK(serie[2] == doctest::Approx(-1e308));
}
//=============================================================================
TEST_CASE("progress delta: window clamped to the recorded samples")
{
    std::vector<Block> blocks { makeBlock("scope1", "scope", 0, 1) };
    std::vector<BlockState> states(1);
    states[0].scopeSeries = { { 1.0, 2.0 } };
    const std::vector<double> time { 0.0, 0.1 };
    const std::vector<std::vector<int>> inputIds { { 0 } };

    const json parsed = json::parse(
        buildProgressJson(1, 0.5, 0.1, -3, 99, time, blocks, states, inputIds, nullptr));
    CHECK(parsed["fromSample"] == 0);
    CHECK(parsed["toSample"] == 2);
    CHECK(parsed["scopes"][0]["timesDelta"].size() == 2);
}
//=============================================================================
TEST_CASE("progress delta: series shorter than the time vector are clamped per series")
{
    // A disconnected/width-mismatched scope may hold fewer samples than time.
    std::vector<Block> blocks { makeBlock("scope1", "scope", 0, 1) };
    std::vector<BlockState> states(1);
    states[0].scopeSeries = { { 1.0 } };
    const std::vector<double> time { 0.0, 0.1, 0.2 };
    const std::vector<std::vector<int>> inputIds { { 0 } };

    const json parsed = json::parse(
        buildProgressJson(1, 0.5, 0.2, 0, 3, time, blocks, states, inputIds, nullptr));
    CHECK(parsed["scopes"][0]["timesDelta"].size() == 3);
    CHECK(parsed["scopes"][0]["seriesDelta"][0].size() == 1);
}
//=============================================================================
TEST_CASE("progress delta: dashboardScope stream is delta-encoded per block")
{
    std::vector<Block> blocks { makeBlock("dash", "dashboardScope", 0, 1) };
    std::vector<BlockState> states(1);
    states[0].scalar2 = 1.0;
    states[0].scopeSeries = { { 1.0, 2.0, 3.0, 4.0 } };
    states[0].xSeries = { 0.0, 0.1, 0.2, 0.3 };
    const std::vector<double> time { 0.0, 0.1, 0.2, 0.3 };
    const std::vector<std::vector<int>> inputIds { { 0 } };

    ProgressDeltaSession session;
    // First frame publishes everything recorded so far...
    json first = json::parse(buildProgressJson(
        1, 0.5, 0.3, 0, 4, time, blocks, states, inputIds, &session.dashboardPublishedLen));
    REQUIRE(first["dashboardUpdates"].size() == 1);
    CHECK(first["dashboardUpdates"][0]["delta"] == true);
    CHECK(first["dashboardUpdates"][0]["times"].size() == 4);
    CHECK(first["dashboardUpdates"][0]["series"][0].size() == 4);

    // ...then two more samples arrive; the next frame carries only those.
    states[0].scopeSeries[0].push_back(5.0);
    states[0].scopeSeries[0].push_back(6.0);
    states[0].xSeries.push_back(0.4);
    states[0].xSeries.push_back(0.5);
    json second = json::parse(buildProgressJson(
        2, 0.9, 0.5, 4, 6, time, blocks, states, inputIds, &session.dashboardPublishedLen));
    CHECK(second["dashboardUpdates"][0]["times"].size() == 2);
    CHECK(second["dashboardUpdates"][0]["series"][0].size() == 2);
    CHECK(second["dashboardUpdates"][0]["series"][0][0] == doctest::Approx(5.0));
}
//=============================================================================
TEST_CASE("final result: dashboardScope stream is bounded to its TimeSpan window")
{
    Block dash = makeBlock("dash", "dashboardScope", 0, 1);
    asJson(dash.params) = json { { "TimeSpan", 2.0 } };
    std::vector<Block> blocks { dash };
    std::vector<BlockState> states(1);
    states[0].scalar2 = 1.0;
    std::vector<double> time;
    for (int k = 0; k <= 100; ++k) { // 10 s of samples at dt = 0.1
        const double t = 0.1 * k;
        time.push_back(t);
        states[0].xSeries.push_back(t);
    }
    states[0].scopeSeries.assign(1, {});
    for (std::size_t k = 0; k < states[0].xSeries.size(); ++k) {
        states[0].scopeSeries[0].push_back((double)k);
    }
    const std::vector<std::vector<int>> inputIds { { 0 } };

    const json parsed
        = json::parse(buildOutputJson("done", "", time, blocks, states, inputIds, ""));
    const auto& update = parsed["dashboardUpdates"][0];
    // Only the last TimeSpan seconds survive: times in [8.0, 10.0].
    REQUIRE(update["times"].size() == update["series"][0].size());
    CHECK(update["times"].size() <= 22);
    CHECK(update["times"][0].get<double>() >= 8.0 - 1e-12);
    CHECK(update["times"][update["times"].size() - 1] == doctest::Approx(10.0));
    // The kept values are the TAIL of the recorded stream, aligned with times.
    CHECK(update["series"][0][update["series"][0].size() - 1] == doctest::Approx(100.0));

    // Without TimeSpan the full stream is kept.
    asJson(blocks[0].params) = json::object();
    const json full = json::parse(buildOutputJson("done", "", time, blocks, states, inputIds, ""));
    CHECK(full["dashboardUpdates"][0]["times"].size() == 101);
}
//=============================================================================
TEST_CASE("progress delta: sequence is stamped verbatim")
{
    std::vector<Block> blocks;
    std::vector<BlockState> states;
    const std::vector<double> time;
    const std::vector<std::vector<int>> inputIds;
    const json parsed = json::parse(buildProgressJson(
        123456789012345LL, 0.0, 0.0, 0, 0, time, blocks, states, inputIds, nullptr));
    CHECK(parsed["sequence"].get<long long>() == 123456789012345LL);
    CHECK(parsed["status"] == "running");
}
//=============================================================================
