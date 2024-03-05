//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include <vector>
//=============================================================================
namespace Nelson {
//=============================================================================
/**
 * @brief Represents a point in a contour line.
 */
class contourPoint
{
public:
    double x; /**< x-coordinate of the point */
    double y; /**< y-coordinate of the point */

    /**
     * @brief Constructor for contourPoint.
     *
     * @param a The x-coordinate.
     * @param b The y-coordinate.
     */
    inline contourPoint(double a, double b) : x(a), y(b) {};
};
//=============================================================================
typedef std::vector<contourPoint> contourLine; /**< A line segment of a contour */
typedef std::vector<contourLine> contourLineSet; /**< A set of contour lines */
typedef std::vector<contourLineSet>
    contourLineCollection; /**< A collection of sets of contour lines */
//=============================================================================
}
//=============================================================================
