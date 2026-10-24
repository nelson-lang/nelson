//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "test_helpers.hpp"
#include "FmuComponent.hpp"
#include "StateLayout.hpp"
#include <cmath>
#include <memory>
#include <utility>
//=============================================================================
using namespace nflow_tests;
//=============================================================================
using Nelson::NFlow::buildStateLayout;
using Nelson::NFlow::IFmuMeComponent;
using Nelson::NFlow::registerFmuMeComponentFactory;
using Nelson::NFlow::SimDiagnostic;
using Nelson::NFlow::StateLayout;
//=============================================================================
// A synthetic Model Exchange component with bouncing-ball dynamics, so the
// fmuMe block + engine wiring can be exercised without fmi4c / a real FMU.
//   states: [h, v]   xdot = [v, -g]
//   outputs: [h, v]
//   1 event indicator g0 = h; on a crossing the velocity is reflected (h reset
//   to 0), modelling the bounce.
//=============================================================================
namespace {
constexpr double kG = 9.81;
constexpr double kRestitution = 0.8;
class MockBouncingBall : public IFmuMeComponent
{
public:
    bool
    initialize(double, double, const std::vector<std::string>&, const std::vector<double>&,
        SimDiagnostic&) override
    {
        m_initialized = true;
        return true;
    }
    int
    numContinuousStates() const override
    {
        return 2;
    }
    int
    numEventIndicators() const override
    {
        return 1;
    }
    void
    getInitialStates(double* x, int n) const override
    {
        if (n > 0) {
            x[0] = 1.0;
        }
        if (n > 1) {
            x[1] = 0.0;
        }
    }
    int
    numInputs() const override
    {
        return 0;
    }
    int
    numOutputs() const override
    {
        return 2;
    }
    bool
    setInputs(const double*, int, SimDiagnostic&) override
    {
        return true;
    }
    bool
    getOutputs(double* out, int n, SimDiagnostic&) override
    {
        if (n > 0) {
            out[0] = m_h;
        }
        if (n > 1) {
            out[1] = m_v;
        }
        return true;
    }
    bool
    setStateTime(double time, const double* x, int n, SimDiagnostic&) override
    {
        m_t = time;
        if (n > 0) {
            m_h = x[0];
        }
        if (n > 1) {
            m_v = x[1];
        }
        return true;
    }
    bool
    getDerivatives(double* xdot, int n, SimDiagnostic&) override
    {
        if (n > 0) {
            xdot[0] = m_v;
        }
        if (n > 1) {
            xdot[1] = -kG;
        }
        return true;
    }
    bool
    getEventIndicators(double* g, int n, SimDiagnostic&) override
    {
        if (n > 0) {
            g[0] = m_h;
        }
        return true;
    }
    bool
    handleEvent(
        const double* xin, double* xout, int n, bool& statesChanged, SimDiagnostic&) override
    {
        const double h = (n > 0) ? xin[0] : 0.0;
        const double v = (n > 1) ? xin[1] : 0.0;
        if (h <= 1e-9 && v < 0.0) {
            if (n > 0) {
                xout[0] = 0.0;
            }
            if (n > 1) {
                xout[1] = -kRestitution * v;
            }
            statesChanged = true;
        } else {
            for (int i = 0; i < n; ++i) {
                xout[i] = xin[i];
            }
            statesChanged = false;
        }
        return true;
    }

private:
    bool m_initialized = false;
    double m_t = 0.0;
    double m_h = 0.0;
    double m_v = 0.0;
};
//=============================================================================
// A synthetic Model Exchange component driven by its input: one state, one
// input, one output, with xdot = u and y = x. It checks that the block gathers
// its input port and pushes it to the FMU before reading derivatives.
class MockInputIntegrator : public IFmuMeComponent
{
public:
    bool
    initialize(double, double, const std::vector<std::string>&, const std::vector<double>&,
        SimDiagnostic&) override
    {
        return true;
    }
    int
    numContinuousStates() const override
    {
        return 1;
    }
    int
    numEventIndicators() const override
    {
        return 0;
    }
    void
    getInitialStates(double* x, int n) const override
    {
        if (n > 0) {
            x[0] = 0.0;
        }
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
    setInputs(const double* values, int n, SimDiagnostic&) override
    {
        m_u = (n > 0) ? values[0] : 0.0;
        return true;
    }
    bool
    getOutputs(double* out, int n, SimDiagnostic&) override
    {
        if (n > 0) {
            out[0] = m_x;
        }
        return true;
    }
    bool
    setStateTime(double, const double* x, int n, SimDiagnostic&) override
    {
        if (n > 0) {
            m_x = x[0];
        }
        return true;
    }
    bool
    getDerivatives(double* xdot, int n, SimDiagnostic&) override
    {
        if (n > 0) {
            xdot[0] = m_u;
        }
        return true;
    }
    bool
    getEventIndicators(double*, int, SimDiagnostic&) override
    {
        return true;
    }
    bool
    handleEvent(
        const double* xin, double* xout, int n, bool& statesChanged, SimDiagnostic&) override
    {
        for (int i = 0; i < n; ++i) {
            xout[i] = xin[i];
        }
        statesChanged = false;
        return true;
    }

private:
    double m_u = 0.0;
    double m_x = 0.0;
};
//=============================================================================
class MockFailingMe : public IFmuMeComponent
{
public:
    explicit MockFailingMe(std::string op) : m_failOp(std::move(op)) { }
    bool
    initialize(double, double, const std::vector<std::string>&, const std::vector<double>&,
        SimDiagnostic&) override
    {
        return true;
    }
    int
    numContinuousStates() const override
    {
        return 1;
    }
    int
    numEventIndicators() const override
    {
        return 1;
    }
    void
    getInitialStates(double* x, int n) const override
    {
        if (n > 0) {
            x[0] = 1.0;
        }
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
        return maybeFail("setInputs", diag);
    }
    bool
    getOutputs(double* out, int n, SimDiagnostic& diag) override
    {
        if (!maybeFail("getOutputs", diag)) {
            return false;
        }
        if (n > 0) {
            out[0] = 1.0;
        }
        return true;
    }
    bool
    setStateTime(double, const double*, int, SimDiagnostic& diag) override
    {
        return maybeFail("setStateTime", diag);
    }
    bool
    getDerivatives(double* xdot, int n, SimDiagnostic& diag) override
    {
        if (!maybeFail("getDerivatives", diag)) {
            return false;
        }
        if (n > 0) {
            xdot[0] = 0.0;
        }
        return true;
    }
    bool
    getEventIndicators(double* g, int n, SimDiagnostic& diag) override
    {
        if (!maybeFail("getEventIndicators", diag)) {
            return false;
        }
        if (n > 0) {
            g[0] = 1.0;
        }
        return true;
    }
    bool
    handleEvent(
        const double* xin, double* xout, int n, bool& statesChanged, SimDiagnostic& diag) override
    {
        if (!maybeFail("handleEvent", diag)) {
            return false;
        }
        for (int i = 0; i < n; ++i) {
            xout[i] = xin[i];
        }
        statesChanged = false;
        return true;
    }

private:
    bool
    maybeFail(const std::string& op, SimDiagnostic& diag) const
    {
        if (m_failOp != op) {
            return true;
        }
        diag.code = "mock_" + op;
        diag.message = op + " failed";
        return false;
    }

    std::string m_failOp;
};
//=============================================================================
// Install the mock factory for a test. The factory is process-global and is not
// restored, so every test that needs it installs it again.
void
installMockMeFactory()
{
    registerFmuMeComponentFactory(
        [](const std::string&, SimDiagnostic&) -> std::unique_ptr<IFmuMeComponent> {
            return std::make_unique<MockBouncingBall>();
        });
}
//=============================================================================
json
fmuMeParams()
{
    json p = json::object();
    p["path"] = "mock.fmu";
    p["numContinuousStates"] = 2;
    p["numEventIndicators"] = 1;
    p["outputWidths"] = json::array({ 1, 1 });
    return p;
}
//=============================================================================
void
installInputIntegratorFactory()
{
    registerFmuMeComponentFactory(
        [](const std::string&, SimDiagnostic&) -> std::unique_ptr<IFmuMeComponent> {
            return std::make_unique<MockInputIntegrator>();
        });
}
//=============================================================================
void
installFailingMeFactory(const std::string& op)
{
    registerFmuMeComponentFactory(
        [op](const std::string&, SimDiagnostic&) -> std::unique_ptr<IFmuMeComponent> {
            return std::make_unique<MockFailingMe>(op);
        });
}
//=============================================================================
json
failingMeParams()
{
    json p = json::object();
    p["path"] = "mock.fmu";
    p["numContinuousStates"] = 1;
    p["numEventIndicators"] = 1;
    p["outputWidths"] = json::array({ 1 });
    return p;
}
//=============================================================================
} // namespace
//=============================================================================
TEST_CASE("fmuMe INIT loads the component and seeds initial continuous states")
{
    ensureHandlersRegistered();
    installMockMeFactory();
    Harness h;
    BlockId fmu = h.addBlock("me1", nflow::kFmuMe, 0, 2, fmuMeParams());
    h.run(fmu, Phase::INIT);
    // The component is stored on the block, and its initial states are latched
    // into BlockState::vec for the engine's initial-state gather.
    CHECK(h.blockState[fmu].external != nullptr);
    REQUIRE(h.blockState[fmu].vec.size() == 2);
    checkClose(h.blockState[fmu].vec[0], 1.0);
    checkClose(h.blockState[fmu].vec[1], 0.0);
}
//=============================================================================
TEST_CASE("fmuMe joins the global continuous-state and zero-crossing layout")
{
    ensureHandlersRegistered();
    installMockMeFactory();
    Harness h;
    BlockId integ = h.addBlock("i1", "integrator", 1, 1, json::object());
    BlockId fmu = h.addBlock("me1", nflow::kFmuMe, 0, 2, fmuMeParams());
    h.run(fmu, Phase::INIT);

    StateLayout layout
        = buildStateLayout(h.blocks, h.blockState, BlockRegistry::instance(), h.variables);

    // integrator (1) + fmuMe (2) = 3 continuous states; fmuMe brings 1 crossing.
    CHECK(layout.totalContinuous == 3);
    CHECK(layout.totalZeroCrossings == 1);
    CHECK(h.blockState[integ].xOffset == 0);
    CHECK(h.blockState[fmu].xOffset == 1);
    CHECK(h.blockState[fmu].xLength == 2);
    CHECK(h.blockState[fmu].gOffset == 0);
    CHECK(h.blockState[fmu].gLength == 1);
}
//=============================================================================
TEST_CASE("fmuMe DERIVATIVE writes xdot = [v, -g] from the global state slice")
{
    ensureHandlersRegistered();
    installMockMeFactory();
    Harness h;
    BlockId fmu = h.addBlock("me1", nflow::kFmuMe, 0, 2, fmuMeParams());
    h.run(fmu, Phase::INIT);
    h.blockState[fmu].xOffset = 0;
    h.blockState[fmu].xLength = 2;
    h.x = { 0.5, -3.0 };
    h.xdot = { 0.0, 0.0 };
    h.run(fmu, Phase::DERIVATIVE);
    checkClose(h.xdot[0], -3.0);
    checkClose(h.xdot[1], -kG);
}
//=============================================================================
TEST_CASE("fmuMe ZERO_CROSSING exposes the height as the event surface")
{
    ensureHandlersRegistered();
    installMockMeFactory();
    Harness h;
    BlockId fmu = h.addBlock("me1", nflow::kFmuMe, 0, 2, fmuMeParams());
    h.run(fmu, Phase::INIT);
    h.blockState[fmu].xOffset = 0;
    h.blockState[fmu].xLength = 2;
    h.blockState[fmu].gOffset = 0;
    h.blockState[fmu].gLength = 1;
    h.x = { 0.25, -2.0 };
    h.g = { 0.0 };
    h.run(fmu, Phase::ZERO_CROSSING);
    checkClose(h.g[0], 0.25); // surface = height
}
//=============================================================================
TEST_CASE("fmuMe EVENT reflects the velocity and resets the height in place")
{
    ensureHandlersRegistered();
    installMockMeFactory();
    Harness h;
    BlockId fmu = h.addBlock("me1", nflow::kFmuMe, 0, 2, fmuMeParams());
    h.run(fmu, Phase::INIT);
    h.blockState[fmu].xOffset = 0;
    h.blockState[fmu].xLength = 2;
    h.x = { 0.0, -4.0 }; // ball hits the floor moving down at 4 m/s
    h.run(fmu, Phase::EVENT);
    checkClose(h.x[0], 0.0);
    checkClose(h.x[1], kRestitution * 4.0); // velocity reflected: +3.2 m/s
}
//=============================================================================
TEST_CASE("fmuMe DERIVATIVE pushes the wired input port to the FMU (xdot = u)")
{
    ensureHandlersRegistered();
    installInputIntegratorFactory();
    Harness h;
    json p = json::object();
    p["path"] = "mock.fmu";
    p["numContinuousStates"] = 1;
    p["numEventIndicators"] = 0;
    p["outputWidths"] = json::array({ 1 });
    BlockId fmu = h.addBlock("me1", nflow::kFmuMe, 1, 1, p);
    h.connect(h.addInput(4.0), fmu);
    h.run(fmu, Phase::INIT);
    h.blockState[fmu].xOffset = 0;
    h.blockState[fmu].xLength = 1;
    h.x = { 0.0 };
    h.xdot = { 0.0 };
    h.run(fmu, Phase::DERIVATIVE);
    // The block gathered its input port and pushed it to the FMU, whose
    // derivative is xdot = u; so the wired 4.0 must reach the state slice.
    checkClose(h.xdot[0], 4.0);
}
//=============================================================================
TEST_CASE("fmuMe propagates failures from every runtime FMI operation")
{
    struct Scenario
    {
        const char* op;
        Phase phase;
    };
    const Scenario scenarios[] = {
        { "setStateTime", Phase::OUTPUT },
        { "setInputs", Phase::OUTPUT },
        { "getOutputs", Phase::OUTPUT },
        { "getDerivatives", Phase::DERIVATIVE },
        { "getEventIndicators", Phase::ZERO_CROSSING },
        { "handleEvent", Phase::EVENT },
    };
    for (const auto& s : scenarios) {
        ensureHandlersRegistered();
        installFailingMeFactory(s.op);
        Harness h;
        BlockId src = h.addInput(3.0);
        BlockId fmu = h.addBlock("me_fail", nflow::kFmuMe, 1, 1, failingMeParams());
        h.connect(src, fmu);
        h.run(fmu, Phase::INIT);
        h.blockState[fmu].xOffset = 0;
        h.blockState[fmu].xLength = 1;
        h.blockState[fmu].gOffset = 0;
        h.blockState[fmu].gLength = 1;
        h.x = { 1.0 };
        h.xdot = { 0.0 };
        h.g = { 0.0 };

        h.run(fmu, s.phase);

        CHECK(h.diagnostics.aborted());
        REQUIRE_FALSE(h.diagnostics.all().empty());
        CHECK(h.diagnostics.all().back().code == std::string("mock_") + s.op);
        CHECK(h.diagnostics.all().back().blockId == "me_fail");
    }
    registerFmuMeComponentFactory(nullptr);
}
//=============================================================================
