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
#include "NelsonWorkspaceBridge.hpp"
//=============================================================================
using Nelson::NFlow::callNelsonFunction;
using Nelson::NFlow::hasNelsonWorkspaceBridge;
using Nelson::NFlow::NelsonCallResult;
using Nelson::NFlow::readWorkspaceSignal;
using Nelson::NFlow::setNelsonWorkspaceBridge;
using Nelson::NFlow::WorkspacePayload;
using Nelson::NFlow::WorkspaceSignal;
using Nelson::NFlow::writeWorkspaceVariable;
TEST_CASE("workspace bridge reports not-installed with a clear diagnostic")
{
    setNelsonWorkspaceBridge({}, {}, {});
    CHECK_FALSE(hasNelsonWorkspaceBridge());

    NelsonCallResult r = callNelsonFunction("2*u", { 3.0 });
    CHECK_FALSE(r.ok);
    CHECK(r.errorMessage.find("not installed") != std::string::npos);

    WorkspaceSignal s = readWorkspaceSignal("simin");
    CHECK_FALSE(s.ok);
    CHECK(s.errorMessage.find("not installed") != std::string::npos);

    std::string err;
    WorkspacePayload p;
    p.variableName = "simout";
    CHECK_FALSE(writeWorkspaceVariable(p, err));
    CHECK(err.find("not installed") != std::string::npos);
}
//=============================================================================
TEST_CASE("workspace bridge routes calls to the installed callbacks")
{
    std::string lastFcn;
    std::vector<double> lastU;
    std::string lastReadVar;
    WorkspacePayload lastPayload;

    setNelsonWorkspaceBridge(
        [&](const std::string& fcn, const std::vector<double>& u,
            const std::vector<double>* /*uImag*/) {
            lastFcn = fcn;
            lastU = u;
            NelsonCallResult r;
            r.ok = true;
            r.y.resize(u.size());
            for (size_t i = 0; i < u.size(); ++i) {
                r.y[i] = 2.0 * u[i]; // pretend the fcn is 2*u
            }
            return r;
        },
        [&](const std::string& name) {
            lastReadVar = name;
            WorkspaceSignal s;
            s.ok = true;
            s.width = 1;
            s.times = { 0.0, 1.0 };
            s.values = { 10.0, 20.0 };
            return s;
        },
        [&](const WorkspacePayload& payload, std::string& err) {
            lastPayload = payload;
            err.clear();
            return true;
        });

    CHECK(hasNelsonWorkspaceBridge());

    NelsonCallResult r = callNelsonFunction("2*u", { 4.0, 5.0 });
    CHECK(r.ok);
    CHECK(lastFcn == "2*u");
    REQUIRE(r.y.size() == 2);
    CHECK(r.y[0] == doctest::Approx(8.0));
    CHECK(r.y[1] == doctest::Approx(10.0));

    WorkspaceSignal s = readWorkspaceSignal("mysignal");
    CHECK(s.ok);
    CHECK(lastReadVar == "mysignal");
    REQUIRE(s.values.size() == 2);
    CHECK(s.values[1] == doctest::Approx(20.0));

    std::string err;
    WorkspacePayload p;
    p.variableName = "simout";
    p.saveFormat = "Structure With Time";
    p.width = 1;
    p.times = { 0.0, 0.5 };
    p.values = { 1.0, 2.0 };
    CHECK(writeWorkspaceVariable(p, err));
    CHECK(lastPayload.variableName == "simout");
    CHECK(lastPayload.saveFormat == "Structure With Time");
    REQUIRE(lastPayload.values.size() == 2);

    // Leave the global bridge cleared for other test cases.
    setNelsonWorkspaceBridge({}, {}, {});
    CHECK_FALSE(hasNelsonWorkspaceBridge());
}
//=============================================================================
