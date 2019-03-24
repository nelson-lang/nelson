//=============================================================================
// Copyright (c) 2016-2019 Allan CORNET (Nelson)
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
    auto x = static_cast<int64_t>(std::floor(t));
    return ((static_cast<double>(x) == t) || !IsFinite(t));
}
//=============================================================================
bool
IsIntegerForm(float t)
{
    auto x = static_cast<int64_t>(std::floor(t));
    return ((static_cast<float>(x) == t) || !IsFinite(t));
}
//=============================================================================
bool
IsIntegerForm(const float* t, size_t nbElements)
{
    if (t != nullptr) {
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
    if (t != nullptr) {
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
