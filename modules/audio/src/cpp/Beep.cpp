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
#define WIN32_LEAN_AND_MEAN
#include <Windows.h>
#endif
#include <iostream>
#include <cstdio>
#include "Beep.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static bool beepOn = true;
//=============================================================================
bool
setBeepOn()
{
    bool previous = beepOn;
    beepOn = true;
    return previous;
}
//=============================================================================
bool
setBeepOff()
{
    bool previous = beepOn;
    beepOn = false;
    return previous;
}
//=============================================================================
bool
getBeepState()
{
    return beepOn;
}
//=============================================================================
bool
beep()
{
    if (beepOn) {
#ifdef _MSC_VER
        MessageBeep(static_cast<UINT>(-1));
#else
        std::cout << "\a" << std::flush;
#endif
    }
    return false;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
