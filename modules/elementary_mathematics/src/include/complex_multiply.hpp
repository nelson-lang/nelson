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
#pragma once
//=============================================================================
#include <complex>
//=============================================================================
template <class T>
std::complex<T>
complex_multiply(std::complex<T> A, std::complex<T> B)
{
    std::complex<T> C;
    if ((A.imag() == 0) && (B.imag() == 0)) {
        C.real(A.real() * B.real());
        C.imag(0);
    } else if (A.imag() == 0) {
        C.real(A.real() * B.real());
        C.imag(A.real() * B.imag());
    } else if (B.imag() == 0) {
        C.real(B.real() * A.real());
        C.imag(B.real() * A.imag());
    } else {
        C.real(A.real() * B.real() - A.imag() * B.imag());
        C.imag(A.real() * B.imag() + A.imag() * B.real());
    }
    return C;
}
//=============================================================================
