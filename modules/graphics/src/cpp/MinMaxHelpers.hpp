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
#include <vector>
#include "ArrayOf.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
double
findVectorMin(const std::vector<double>& v);
//=============================================================================
double
findVectorMax(const std::vector<double>& v);
//=============================================================================
double
ArrayOfMin(const ArrayOf& x);
//=============================================================================
double
ArrayOfMax(const ArrayOf& x);
//=============================================================================
// Bounding box of the points whose coordinates are all finite.
class PointsExtent
{
public:
    void
    add(double x, double y, double z);
    // Pushes [xmin xmax ymin ymax zmin zmax]; [+inf -inf] pairs when no point counted.
    void
    appendTo(std::vector<double>& limits) const;

private:
    double box[6] = { 0, 0, 0, 0, 0, 0 };
    bool empty = true;
};
//=============================================================================
// Extent of the points (xs[i], ys[i], zs[i]); a missing z is 0.
void
appendPointsExtent(std::vector<double>& limits, const std::vector<double>& xs,
    const std::vector<double>& ys, const std::vector<double>& zs);
//=============================================================================
} // namespace Nelson
//=============================================================================
