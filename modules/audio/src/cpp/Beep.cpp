//=============================================================================
// Copyright (c) 2016-2018 Allan CORNET (Nelson)
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#ifdef _MSC_VER
#include <Windows.h>
#endif
#include "Beep.hpp"
#include <iostream>
#include <stdio.h>
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
        MessageBeep((UINT)-1);
#else
        std::cout << "\a" << std::flush;
#endif
    }
    return false;
}
//=============================================================================
}
//=============================================================================
