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
#pragma once
//=============================================================================
#include <cmath>
#include <algorithm>
#include "ArrayOf.hpp"
//=============================================================================
template<typename T>
T saturate(T val, T min, T max)
{
    return std::min(std::max(val, min), max);
}
//=============================================================================
template<typename TOUT, typename TIN>
TOUT RealToIntX(TIN a)
{
    TIN A = (TIN)round(a);
    if (!std::isfinite(A))
    {
        if (std::isnan(A))
        {
            return (TOUT)0;
        }
        if (A > 0)
        {
            return (TOUT)std::numeric_limits<TOUT>::max();
        }
        return (TOUT)std::numeric_limits<TOUT>::min();
    }
    return (TOUT)saturate(A, (TIN)std::numeric_limits<TOUT>::min(), (TIN)std::numeric_limits<TOUT>::max());
}
//=============================================================================
