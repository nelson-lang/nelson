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
