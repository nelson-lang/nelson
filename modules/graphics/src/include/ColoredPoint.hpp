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
class coloredPoint
{
public:
    //=============================================================================
    double x;
    double y;
    double z;
    double r;
    double g;
    double b;
    double a;
    double nx;
    double ny;
    double nz;
    // Texture coordinates in [0, 1], used only when a surface texture is bound on
    // the renderer (FaceColor 'texturemap'); ignored otherwise.
    double u;
    double v;
    //=============================================================================
    coloredPoint(double _x, double _y, double _z, double _r, double _g, double _b, double _a,
        double _nx = 0, double _ny = 0, double _nz = 1, double _u = 0, double _v = 0)
    {
        x = _x;
        y = _y;
        z = _z;
        r = _r;
        g = _g;
        b = _b;
        a = _a;
        nx = _nx;
        ny = _ny;
        nz = _nz;
        u = _u;
        v = _v;
    }
    //=============================================================================
};
//=============================================================================
