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
class RGBAColorData
{
    //=============================================================================
public:
    double r;
    double g;
    double b;
    double a;
    RGBAColorData()
    {
        r = 0;
        g = 0;
        b = 0;
        a = 0;
    };
    RGBAColorData(double red, double green, double blue, double alpha)
        : r(red), g(green), b(blue), a(alpha) {};
};
//=============================================================================
