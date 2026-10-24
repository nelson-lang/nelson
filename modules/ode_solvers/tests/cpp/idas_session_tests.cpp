//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Validates OdeCore::IdasSession on an index-1 DAE with a closed
// form:
//     x' = -x                (differential)
//     0  = y - x^2           (algebraic)
// so x(t) = x0 e^{-t} and y(t) = x(t)^2. The residual is
//     r[0] = x' + x ,  r[1] = y - x^2 ,  id = [1, 0].
// A correct IdasSession integrates the differential state and holds the
// algebraic state on its constraint manifold, with IDACalcIC seeding consistent
// initial conditions.
//=============================================================================
#include "OdeCore.hpp"
#include "doctest.h"
#include <cmath>
#include <string>
#include <vector>
//=============================================================================
using namespace Nelson::OdeCore;
//=============================================================================
TEST_CASE("IdasSession integrates an index-1 DAE (x'=-x, y=x^2)")
{
    IdasSession session;

    ResidualFn residual = [](double, const double* y, const double* yp, double* r) -> bool {
        r[0] = yp[0] + y[0]; // x' + x = 0  -> x' = -x
        r[1] = y[1] - y[0] * y[0]; // y - x^2 = 0
        return true;
    };

    const std::vector<double> y0 { 1.0, 1.0 }; // x0 = 1, y0 = x0^2 = 1 (consistent)
    const std::vector<double> yp0 { 0.0, 0.0 }; // guess; IDACalcIC refines
    IdasOptions opts;
    opts.relTol = 1e-9;
    opts.absTol = { 1e-10 };
    opts.id = { 1.0, 0.0 }; // x differential, y algebraic

    std::string err;
    REQUIRE(session.init(residual, RootFn {}, y0, yp0, 0.0, 0.1, opts, err));
    CHECK(err.empty());

    std::vector<double> y(2, 0.0);
    std::vector<double> yp(2, 0.0);

    // Integrate to t = 0.5 then t = 1.0 (advanceTo continues from the last stop).
    IdasSession::AdvanceOut out1 = session.advanceTo(0.5, y.data(), yp.data());
    CHECK(out1.status == CvodesStatus::Success);
    CHECK(y[0] == doctest::Approx(std::exp(-0.5)).epsilon(1e-6)); // x(0.5)
    CHECK(y[1] == doctest::Approx(std::exp(-1.0)).epsilon(1e-6)); // y = x^2 = e^{-1}

    IdasSession::AdvanceOut out2 = session.advanceTo(1.0, y.data(), yp.data());
    CHECK(out2.status == CvodesStatus::Success);
    CHECK(y[0] == doctest::Approx(std::exp(-1.0)).epsilon(1e-6)); // x(1)
    CHECK(y[1] == doctest::Approx(std::exp(-2.0)).epsilon(1e-6)); // y(1) = x(1)^2
    CHECK(yp[0] == doctest::Approx(-std::exp(-1.0)).epsilon(1e-5)); // x' = -x

    CHECK(session.stats().steps > 0);
}
//=============================================================================
TEST_CASE("IdasSession root finding stops at a crossing")
{
    IdasSession session;

    // x' = -x, x(0) = 1; a root at x = 0.5 (t = ln 2 ~ 0.6931).
    ResidualFn residual = [](double, const double* y, const double* yp, double* r) -> bool {
        r[0] = yp[0] + y[0];
        return true;
    };
    RootFn roots = [](double, const double* y, double* g) -> bool {
        g[0] = y[0] - 0.5;
        return true;
    };

    IdasOptions opts;
    opts.relTol = 1e-9;
    opts.absTol = { 1e-10 };
    opts.id = { 1.0 };
    opts.numRoots = 1;

    std::string err;
    REQUIRE(session.init(residual, roots, { 1.0 }, { -1.0 }, 0.0, 0.1, opts, err));

    std::vector<double> y(1, 0.0);
    IdasSession::AdvanceOut out = session.advanceTo(10.0, y.data(), nullptr);
    CHECK(out.status == CvodesStatus::RootFound);
    CHECK(out.rootsFound.size() == 1);
    CHECK(out.t == doctest::Approx(std::log(2.0)).epsilon(1e-5));
    CHECK(y[0] == doctest::Approx(0.5).epsilon(1e-6));
}
//=============================================================================
