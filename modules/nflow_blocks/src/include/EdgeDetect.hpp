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
#include <cmath>
#include <limits>
//=============================================================================
namespace Nelson {
//=============================================================================
namespace NFlow {
    //=============================================================================
    // One definition of what an edge is, for every block that watches a
    // control signal: a triggered / resettable subsystem, an integrator with
    // an external reset.
    //
    // A control resting exactly at zero is latched as the smallest positive
    // (or negative) double, whose SIGN records the side it arrived from:
    // +side means it rose to zero, -side means it fell to zero. That is what
    // makes a crossing landing exactly ON a sample one edge and not two.
    //=============================================================================
    inline bool
    edgeLatchIsZero(double prev)
    {
        const double side = (std::numeric_limits<double>::min)();
        return prev == side || prev == -side;
    }
    //=============================================================================
    // True when the control crossed zero in the requested direction between
    // the value latched at the previous major step and the current one.
    //   rising (0):  a negative value reaching zero or above, or zero going
    //                strictly positive;
    //   falling (1): a positive value reaching zero or below, or zero going
    //                strictly negative;
    //   either (2):  both.
    // Once the control is parked ON zero its arrival has already fired, so
    // only a strict move to the other side is a new edge.
    //=============================================================================
    inline bool
    edgeFired(double prev, double cur, int triggerType)
    {
        bool rising, falling;
        if (edgeLatchIsZero(prev)) {
            rising = (prev < 0.0) && (cur > 0.0);
            falling = (prev > 0.0) && (cur < 0.0);
        } else {
            rising = (prev < 0.0 && cur >= 0.0) || (prev == 0.0 && cur > 0.0);
            falling = (prev > 0.0 && cur <= 0.0) || (prev == 0.0 && cur < 0.0);
        }
        if (triggerType == 1) {
            return falling;
        }
        if (triggerType == 2) {
            return rising || falling;
        }
        return rising;
    }
    //=============================================================================
    // The control value to remember for the next step's edge test: the value
    // itself, unless it is exactly zero, in which case the side it arrived
    // from.
    //=============================================================================
    inline double
    edgeZeroSideLatch(double prev, double cur, bool havePrev)
    {
        if (cur != 0.0 || !havePrev) {
            return cur;
        }
        if (edgeLatchIsZero(prev)) {
            return prev; // still parked on zero: keep the side it came from
        }
        const double side = (std::numeric_limits<double>::min)();
        if (prev > 0.0) {
            return -side;
        }
        if (prev < 0.0) {
            return side;
        }
        return 0.0; // started at zero, no side yet
    }
    //=============================================================================
    // Edge type of a textual setting, matching the trigger types above.
    // Anything else (including "none") answers -1.
    //=============================================================================
    inline int
    edgeTypeOf(const std::string& name)
    {
        if (name == "rising") {
            return 0;
        }
        if (name == "falling") {
            return 1;
        }
        if (name == "either") {
            return 2;
        }
        return -1;
    }
    //=============================================================================
} // namespace NFlow
//=============================================================================
} // namespace Nelson
//=============================================================================
