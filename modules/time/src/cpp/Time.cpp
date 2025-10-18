//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#ifdef _MSC_VER
#pragma warning(disable : 4244)
#endif
//=============================================================================
#include <chrono>
#include "Time.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
uint64
TimeAsNanoSeconds()
{
    using namespace std::chrono;
    auto now = system_clock::now(); // system_clock is wall-clock time (typically UTC)
    auto ns = duration_cast<nanoseconds>(now.time_since_epoch()).count();
    return static_cast<uint64>(ns);
}
//=============================================================================
double
TimeAsSeconds()
{
    uint64 _now = TimeAsNanoSeconds();
    return double(_now) * 1e-9;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
