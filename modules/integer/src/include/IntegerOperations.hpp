//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
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
#include <type_traits>
#include <limits>
//=============================================================================
namespace Nelson {
//=============================================================================
template <class T>
T
scalarInteger_plus_scalarInteger(T a, T b)
{
    if (std::is_signed<T>()) {
        bool negativeOverflow = a <= std::numeric_limits<T>::max() - b;
        bool positiveOverflow = a >= std::numeric_limits<T>::min() - b;
        if (b < 0 ? positiveOverflow : negativeOverflow) {
            return a + b;
        } else {
            if (positiveOverflow) {
                return std::numeric_limits<T>::max();
            } else {
                return std::numeric_limits<T>::min();
            }
        }
    } else {
        T op = a + b;
        if ((op < a) || (op < b)) {
            return std::numeric_limits<T>::max();
        } else {
            return (T)op;
        }
    }
}
//=============================================================================
template <class T>
T
scalarInteger_minus_scalarInteger(T a, T b)
{
    T op = a - b;
    if ((op < a) != (b > 0)) {
        if (op > 0) {
            return std::numeric_limits<T>::min();
        } else {
            return std::numeric_limits<T>::max();
        }
    } else {
        return (T)op;
    }
}
//=============================================================================
template <class T>
T
scalarInteger_times_scalarInteger(T a, T b)
{
    if (std::is_signed<T>()) {
        if (a > 0 && b > 0 && a > std::numeric_limits<T>::max() / b) {
            return std::numeric_limits<T>::max();
        }
        if (a < 0 && b > 0 && a < std::numeric_limits<T>::min() / b) {
            return std::numeric_limits<T>::min();
        }
        if (a > 0 && b < 0 && b < std::numeric_limits<T>::min() / a) {
            return std::numeric_limits<T>::min();
        }
        if (a < 0 && b < 0
            && (a <= std::numeric_limits<T>::min() || b <= std::numeric_limits<T>::min()
                   || -a > std::numeric_limits<T>::max() / -b)) {
            return std::numeric_limits<T>::max();
        }
        return (T)(a * b);
    }
    if ((a == 0) || (b == 0)) {
        return (T)(0);
    }
    if ((a == 1) || (b == 1)) {
        if (a == 1) {
            return (T)b;
        }
        return (T)a;
    }
    T aa = ((std::numeric_limits<T>::max() / b) * 2)
        + ((((std::numeric_limits<T>::max() % b) * 2) + 1) / b);
    if (a > aa) {
        if (aa > 0) {
            return std::numeric_limits<T>::max();
        }
        return std::numeric_limits<T>::min();
    }
    return (T)(a * b);
}
//=============================================================================
template <class T>
T
scalarInteger_division_scalarInteger(T a, T b)
{
    if (b == 0) {
        if (a > 0) {
            return std::numeric_limits<T>::max();
        } else {
            return std::numeric_limits<T>::min();
        }
    }
    if (a == std::numeric_limits<T>::min() && b == -1) {
        return std::numeric_limits<T>::max();
    }
    if (a == std::numeric_limits<T>::max() && b == -1) {
        return std::numeric_limits<T>::min();
    }
    return (T)(a / b);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
