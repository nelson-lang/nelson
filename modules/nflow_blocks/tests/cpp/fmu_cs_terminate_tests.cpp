//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// A3.1: a Co-Simulation FMU that returns terminateRequested asks the engine
// for a clean early stop. The bridge surfaces it as diag.requestStop; the fmu
// block turns that into ctx.stopRequested (which the engine loop reads to end
// the run and keep the results).
//=============================================================================
#include "test_helpers.hpp"
#include "FmuComponent.hpp"
#include <memory>
#include <utility>
//=============================================================================
using namespace nflow_tests;
//=============================================================================
using Nelson::NFlow::IFmuComponent;
using Nelson::NFlow::registerFmuComponentFactory;
using Nelson::NFlow::SimDiagnostic;
//=============================================================================
namespace {
// A minimal CS component whose step succeeds but asks to terminate; the same
// signal the real bridge raises when the FMU returns terminateRequested.
struct MockTerminatingCs : public IFmuComponent
{
    double lastTol = -1.0; // records the tolerance handed in before initialize()
    void
    setSolverTolerance(double relTol) override
    {
        lastTol = relTol;
    }
    bool
    initialize(double, double, const std::vector<std::string>&, const std::vector<double>&,
        SimDiagnostic&) override
    {
        return true;
    }
    int
    numInputs() const override
    {
        return 0;
    }
    int
    numOutputs() const override
    {
        return 1;
    }
    bool
    setInputs(const double*, int, SimDiagnostic&) override
    {
        return true;
    }
    bool
    getOutputs(double* out, int n, SimDiagnostic&) override
    {
        for (int i = 0; i < n; ++i) {
            out[i] = 0.0;
        }
        return true;
    }
    bool
    doStep(double currentTime, double stepSize, SimDiagnostic& diag) override
    {
        // Step succeeds, but the FMU wants to stop: request a clean early stop.
        diag.requestStop = true;
        diag.code = "fmu_terminate";
        diag.message
            = "mock FMU requested termination at t=" + std::to_string(currentTime + stepSize);
        return true;
    }
};
//=============================================================================
struct MockFailingCs : public IFmuComponent
{
    std::string failOp;
    explicit MockFailingCs(std::string op) : failOp(std::move(op)) { }
    bool
    initialize(double, double, const std::vector<std::string>&, const std::vector<double>&,
        SimDiagnostic&) override
    {
        return true;
    }
    int
    numInputs() const override
    {
        return 1;
    }
    int
    numOutputs() const override
    {
        return 1;
    }
    bool
    setInputs(const double*, int, SimDiagnostic& diag) override
    {
        if (failOp == "setInputs") {
            diag.code = "mock_set_inputs";
            diag.message = "setInputs failed";
            return false;
        }
        return true;
    }
    bool
    getOutputs(double*, int, SimDiagnostic& diag) override
    {
        if (failOp == "getOutputs") {
            diag.code = "mock_get_outputs";
            diag.message = "getOutputs failed";
            return false;
        }
        return true;
    }
    bool
    doStep(double, double, SimDiagnostic& diag) override
    {
        if (failOp == "doStepEarlyReturn") {
            diag.code = "fmu_early_return";
            diag.message = "FMU returned early at t=0.05; fixed-step execution cannot reschedule.";
            return false;
        }
        return true;
    }
};
//=============================================================================
} // namespace
//=============================================================================
TEST_CASE("A3.1: an FMU terminateRequested sets ctx.stopRequested via the block")
{
    ensureHandlersRegistered();
    registerFmuComponentFactory(
        [](const std::string&, SimDiagnostic&) -> std::unique_ptr<IFmuComponent> {
            return std::make_unique<MockTerminatingCs>();
        });

    Harness h;
    json p = json::object();
    p["path"] = "mock.fmu";
    BlockId fmu = h.addBlock("cs1", "fmu", 0, 1, p);
    h.run(fmu, Phase::INIT);
    CHECK(h.blockState[fmu].external != nullptr);
    CHECK_FALSE(h.stopRequested); // nothing requested yet after INIT

    h.run(fmu, Phase::UPDATE);
    CHECK(h.stopRequested);

    // Do not leak the mock factory into other tests.
    registerFmuComponentFactory(nullptr);
}
//=============================================================================
TEST_CASE("fmu CS block propagates getOutputs failure")
{
    ensureHandlersRegistered();
    registerFmuComponentFactory(
        [](const std::string&, SimDiagnostic&) -> std::unique_ptr<IFmuComponent> {
            return std::make_unique<MockFailingCs>("getOutputs");
        });

    Harness h;
    json p = json::object();
    p["path"] = "mock.fmu";
    BlockId fmu = h.addBlock("cs1", "fmu", 0, 1, p);
    h.run(fmu, Phase::INIT);
    h.run(fmu, Phase::OUTPUT);

    CHECK(h.diagnostics.aborted());
    REQUIRE_FALSE(h.diagnostics.all().empty());
    CHECK(h.diagnostics.all().back().code == "mock_get_outputs");
    CHECK(h.diagnostics.all().back().blockId == "cs1");

    registerFmuComponentFactory(nullptr);
}
//=============================================================================
TEST_CASE("fmu CS block propagates setInputs failure")
{
    ensureHandlersRegistered();
    registerFmuComponentFactory(
        [](const std::string&, SimDiagnostic&) -> std::unique_ptr<IFmuComponent> {
            return std::make_unique<MockFailingCs>("setInputs");
        });

    Harness h;
    json p = json::object();
    p["path"] = "mock.fmu";
    BlockId src = h.addInput(2.0);
    BlockId fmu = h.addBlock("cs1", "fmu", 1, 1, p);
    h.connect(src, fmu);
    h.run(fmu, Phase::INIT);
    h.run(fmu, Phase::UPDATE);

    CHECK(h.diagnostics.aborted());
    REQUIRE_FALSE(h.diagnostics.all().empty());
    CHECK(h.diagnostics.all().back().code == "mock_set_inputs");
    CHECK(h.diagnostics.all().back().blockId == "cs1");

    registerFmuComponentFactory(nullptr);
}
//=============================================================================
TEST_CASE("fmu CS block refuses FMI3 earlyReturn reported by the bridge")
{
    ensureHandlersRegistered();
    registerFmuComponentFactory(
        [](const std::string&, SimDiagnostic&) -> std::unique_ptr<IFmuComponent> {
            return std::make_unique<MockFailingCs>("doStepEarlyReturn");
        });

    Harness h;
    json p = json::object();
    p["path"] = "mock.fmu";
    BlockId fmu = h.addBlock("cs1", "fmu", 0, 1, p);
    h.run(fmu, Phase::INIT);
    h.run(fmu, Phase::UPDATE);

    CHECK(h.diagnostics.aborted());
    REQUIRE_FALSE(h.diagnostics.all().empty());
    CHECK(h.diagnostics.all().back().code == "fmu_early_return");
    CHECK(h.diagnostics.all().back().blockId == "cs1");

    registerFmuComponentFactory(nullptr);
}
//=============================================================================
TEST_CASE("A3.2: the diagram tolerance is handed to the FMU before initialize")
{
    ensureHandlersRegistered();
    registerFmuComponentFactory(
        [](const std::string&, SimDiagnostic&) -> std::unique_ptr<IFmuComponent> {
            return std::make_unique<MockTerminatingCs>();
        });

    Harness h;
    h.relTol = 1e-4;
    json p = json::object();
    p["path"] = "mock.fmu";
    BlockId fmu = h.addBlock("cs1", "fmu", 0, 1, p);
    h.run(fmu, Phase::INIT);

    auto* mock = dynamic_cast<MockTerminatingCs*>(h.blockState[fmu].external.get());
    REQUIRE(mock != nullptr);
    checkClose(mock->lastTol, 1e-4); // propagated, not the FMU's default

    registerFmuComponentFactory(nullptr);
}
//=============================================================================
