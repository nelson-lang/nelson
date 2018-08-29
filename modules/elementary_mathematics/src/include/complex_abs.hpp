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
#pragma once
//=============================================================================
template <class T>
T
complex_abs(T real, T imag)
{
    double temp;
    if (real < 0) {
        real = -real;
    }
    if (imag < 0) {
        imag = -imag;
    }
    if (imag > real) {
        temp = real;
        real = imag;
        imag = (T)(temp);
    }
    if ((real + imag) == real) {
        return (real);
    }
    temp = imag / real;
    temp = real * sqrt(1.0 + temp * temp);
    return (T)(temp);
}
//=============================================================================
