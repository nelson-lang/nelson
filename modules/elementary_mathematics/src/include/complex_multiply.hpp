//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
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
