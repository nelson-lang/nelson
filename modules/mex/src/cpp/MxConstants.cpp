//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <cmath>
#include <limits>
#include "matrix.h"
#include "MxHelpers.hpp"
//=============================================================================
bool
mxIsInf(double value)
{
    return std::isinf(value);
}
//=============================================================================
bool
mxIsNaN(double value)
{
    return std::isnan(value);
}
//=============================================================================
bool
mxIsFinite(double value)
{
    return std::isfinite(value);
}
//=============================================================================
double
mxGetNaN(void)
{
    return std::nan("");
}
//=============================================================================
double
mxGetInf(void)
{
    return std::numeric_limits<double>::infinity();
}
//=============================================================================
double
mxGetEps(void)
{
    return std::numeric_limits<double>::epsilon();
}
//=============================================================================
