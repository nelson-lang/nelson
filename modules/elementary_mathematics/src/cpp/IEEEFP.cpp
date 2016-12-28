//=============================================================================
// Copyright (c) 2016-2017 Allan CORNET (Nelson)
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
#include <cmath>
#include <stdint.h>
#include "IEEEFP.hpp"
//=============================================================================
bool IsInfinite(float t)
{
    union
    {
        float f;
        unsigned int i;
    } u;
    u.f = t;
    if (((u.i & 0x7f800000) == 0x7f800000) && ((u.i & 0x007fffff) == 0))
    {
        return true;
    }
    return false;
}
//=============================================================================
bool IsInfinite(double t)
{
    union
    {
        double d;
        unsigned int i[2];
    } u;
    u.d = t;
#if (WORDS_BIGENDIAN!=1)
    if( ((u.i[1] & 0x7ff00000) == 0x7ff00000)
            && (((u.i[1] & 0x000fffff) == 0) && (u.i[0] == 0)))
    {
        return true;
    }
#else
    if( ((u.i[0] & 0x7ff00000) == 0x7ff00000)
            && (((u.i[0] & 0x000fffff) == 0) && (u.i[1] == 0)))
    {
        return true;
    }
#endif
    return false;
}
//=============================================================================
bool IsNaN(int t)
{
    return true;
}
//=============================================================================
bool IsNaN(unsigned int t)
{
    return true;
}
//=============================================================================
bool IsNaN(float t)
{
    union
    {
        float f;
        unsigned int i;
    } u;
    u.f = t;
    if (((u.i & 0x7f800000) == 0x7f800000) && ((u.i & 0x007fffff) != 0))
    {
        return true;
    }
    return false;
}
//=============================================================================
bool IsNaN(double t)
{
    union
    {
        double d;
        unsigned int i[2];
    } u;
    u.d = t;
#if (WORDS_BIGENDIAN!=1)
    if( ((u.i[1] & 0x7ff00000) == 0x7ff00000)
            && (((u.i[1] & 0x000fffff) != 0) || (u.i[0] != 0)))
    {
        return true;
    }
#else
    if( ((u.i[0] & 0x7ff00000) == 0x7ff00000)
            && (((u.i[0] & 0x000fffff) != 0) || (u.i[1] != 0)))
    {
        return true;
    }
#endif
    return false;
}
//=============================================================================
bool IsFinite(float t)
{
    return (!(IsNaN(t) || IsInfinite(t)));
}
//=============================================================================
bool IsFinite(double t)
{
    return (!(IsNaN(t) || IsInfinite(t)));
}
//=============================================================================
bool IsIntegerForm(double t)
{
    int64_t x = (int64_t)std::floor(t);
    return (bool)(((double)x == t) || !IsFinite(t));
}
//=============================================================================
bool IsIntegerForm(float t)
{
    int64_t x = (int64_t)std::floor(t);
    return (bool)(((float)x == t) || !IsFinite(t));
}
//=============================================================================
bool IsIntegerForm(const float *t, size_t nbElements)
{
    if (t)
    {
        for (size_t k = 0; k < nbElements; k++)
        {
            if (!IsIntegerForm(t[k]))
            {
                return false;
            }
        }
        return true;
    }
    return false;
}
//=============================================================================
bool IsIntegerForm(const double *t, size_t nbElements)
{
    if (t)
    {
        for (size_t k = 0; k < nbElements; k++)
        {
            if (!IsIntegerForm(t[k]))
            {
                return false;
            }
        }
        return true;
    }
    return false;
}
//=============================================================================
