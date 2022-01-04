//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// Alternatively, you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of
// the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this program. If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#include "IEEEFP.hpp"
#include <cmath>
#include <cstdint>
//=============================================================================
bool
IsInfinite(float t)
{
    return std::isinf(t);
}
//=============================================================================
bool
IsInfinite(double t)
{
    return std::isinf(t);
}
//=============================================================================
bool
IsNaN(int t)
{
    return true;
}
//=============================================================================
bool
IsNaN(unsigned int t)
{
    return true;
}
//=============================================================================
bool
IsNaN(float t)
{
    return std::isnan(t);
}
//=============================================================================
bool
IsNaN(double t)
{
    return std::isnan(t);
}
//=============================================================================
bool
IsFinite(float t)
{
    return std::isfinite(t);
}
//=============================================================================
bool
IsFinite(double t)
{
    return std::isfinite(t);
}
//=============================================================================
bool
IsIntegerForm(double t)
{
    return (rint((long double)(t)) == (long double)(t) && IsFinite(t));
}
//=============================================================================
bool
IsIntegerForm(float t)
{
    return (rint((long double)(t)) == (long double)(t) && IsFinite(t));
}
//=============================================================================
bool
IsIntegerForm(const float* t, size_t nbElements)
{
    if (t != nullptr && nbElements > 0) {
        for (size_t k = 0; k < nbElements; k++) {
            if (!IsIntegerForm(t[k])) {
                return false;
            }
        }
        return true;
    }
    return false;
}
//=============================================================================
bool
IsIntegerForm(const double* t, size_t nbElements)
{
    if (t != nullptr && nbElements > 0) {
        for (size_t k = 0; k < nbElements; k++) {
            if (!IsIntegerForm(t[k])) {
                return false;
            }
        }
        return true;
    }
    return false;
}
//=============================================================================
bool
IsIntegerFormOrNotFinite(const double* t, size_t nbElements)
{
    if (t != nullptr && nbElements > 0) {
        for (size_t k = 0; k < nbElements; k++) {
            if (!IsIntegerForm(t[k]) && std::isfinite(t[k])) {
                return false;
            }
        }
        return true;
    }
    return false;
}
//=============================================================================
bool
IsIntegerFormOrNotFinite(const float* t, size_t nbElements)
{
    if (t != nullptr && nbElements > 0) {
        for (size_t k = 0; k < nbElements; k++) {
            if (!IsIntegerForm(t[k]) && std::isfinite(t[k])) {
                return false;
            }
        }
        return true;
    }
    return false;
}
//=============================================================================
