//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "Epsilon.hpp"
#include "IEEEFP.hpp"
#include <cfloat>
#include <cmath>
#include <limits>
//=============================================================================
namespace Nelson {
//=============================================================================
double
Epsilon(double X)
{
    if (!std::isfinite(X)) {
        return std::numeric_limits<double>::quiet_NaN();
    }
    double machEps = 0.0;
    if (X == 1.0) {
        machEps = std::numeric_limits<double>::epsilon();
    } else if (X >= 0) {
        double up = std::nextafter(X, DBL_MAX);
        // At the top of the range nextafter cannot go higher; fall back to the
        // spacing toward zero so eps(realmax) is the true ULP, not 0.
        machEps = (up > X) ? (up - X) : (X - std::nextafter(X, 0.0));
    } else {
        double down = std::nextafter(X, -DBL_MAX);
        machEps = (down < X) ? (X - down) : (std::nextafter(X, 0.0) - X);
    }
    return machEps;
}
//=============================================================================
single
Epsilon(single X)
{
    if (!std::isfinite(X)) {
        return std::numeric_limits<single>::quiet_NaN();
    }
    single machEps = 0.0;
    if (fabs(X - 1.0) == 0.0) {
        machEps = std::numeric_limits<single>::epsilon();
    } else if (X >= 0) {
        single up = std::nextafter(X, static_cast<single>(FLT_MAX));
        machEps = (up > X) ? (up - X) : (X - std::nextafter(X, static_cast<single>(0)));
    } else {
        single down = std::nextafter(X, static_cast<single>(-FLT_MAX));
        machEps = (down < X) ? (X - down) : (std::nextafter(X, static_cast<single>(0)) - X);
    }
    return machEps;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
