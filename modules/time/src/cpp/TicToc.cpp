//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "TicToc.hpp"
#include <boost/chrono/chrono.hpp>
//=============================================================================
namespace Nelson {
//=============================================================================
static uint64
nowAsNanoseconds()
{
    boost::chrono::nanoseconds ns = boost::chrono::high_resolution_clock::now().time_since_epoch();
    return uint64(static_cast<boost::uint64_t>(ns.count()));
}
//=============================================================================
bool
Tic(Evaluator* eval)
{
    eval->TimerValue = nowAsNanoseconds();
    return true;
}
//=============================================================================
bool
Toc(Evaluator* eval, double& tValue)
{
    return Toc(eval->TimerValue, tValue);
}
//=============================================================================
bool
Toc(uint64 t, double& tValue)
{
    tValue = double(nowAsNanoseconds() - t) * 1e-9;
    return true;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
