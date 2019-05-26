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
#include "Epsilon.hpp"
#include "IEEEFP.hpp"
#include <cfloat>
#include <limits>
//=============================================================================
namespace Nelson {
//=============================================================================
template <class T>
T
nextafter(T x, T y)
{
    union
    {
        T f;
        unsigned int i;
    } u;
    if (IsNaN(y) || IsNaN(x)) {
        return x + y;
    }
    if (x == y) {
        return y;
    }
    u.f = x;
    if (x == 0.0) {
        u.i = 1;
        return y > 0.0 ? u.f : -u.f;
    }
    if (((x > 0.0) ^ (y > x)) == 0) {
        u.i++;
    } else {
        u.i--;
    }
    return u.f;

    return 0.0;
}
//=============================================================================
double
Epsilon(double X)
{
    double machEps = 0.0;
    if (X == 1.0) {
        machEps = std::numeric_limits<double>::epsilon();
    } else {
        if (X >= 0) {
            machEps = nextafter<double>(X, DBL_MAX) - X;
        } else {
            machEps = X - nextafter<double>(X, -DBL_MAX);
        }
    }
    return machEps;
}
//=============================================================================
single
Epsilon(single X)
{
    single machEps = 0.0;
    if (fabs(X - 1.0) == 0.0) {
        machEps = std::numeric_limits<single>::epsilon();
    } else {
        if (X >= 0) {
            machEps = nextafter<single>(X, static_cast<single>(FLT_MAX)) - X;
        } else {
            machEps = X - nextafter<single>(X, static_cast<single>(-FLT_MAX));
        }
    }
    return machEps;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
