//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <chrono>
#include "TicToc.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static uint64
nowAsNanoseconds()
{
    std::chrono::nanoseconds ns = std::chrono::high_resolution_clock::now().time_since_epoch();
    return uint64(static_cast<std::uint64_t>(ns.count()));
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
