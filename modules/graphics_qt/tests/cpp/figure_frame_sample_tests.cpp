//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "FigureFrameSample.hpp"
#include <cmath>
#include <iostream>
//=============================================================================
int
main()
{
    FigureFrameSample sample(2, 3);
    bool valid = !sample.complete() && sample.frames() == 0 && sample.fps() == 0;
    valid &= !sample.frame(1000000000);
    valid &= !sample.frame(2000000000);
    valid &= sample.frames() == 0;
    valid &= !sample.frame(2010000000);
    valid &= !sample.frame(2030000000);
    valid &= sample.frame(2050000000);
    valid &= sample.complete() && sample.frames() == 3;
    valid &= sample.duration() == 50000000 && std::abs(sample.fps() - 60.0) < 1e-12;
    valid &= sample.frame(9000000000);
    valid &= sample.frames() == 3 && sample.duration() == 50000000;
    FigureFrameSample noElapsedTime(1, 1);
    valid &= !noElapsedTime.frame(100);
    valid &= noElapsedTime.frame(100) && noElapsedTime.fps() == 0;
    if (!valid) {
        std::cerr << "Figure presentation sampling regression\n";
    }
    return valid ? 0 : 1;
}
//=============================================================================
