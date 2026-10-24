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
#include "nlsGeometry_exports.h"
#include <string>
#include <vector>
//=============================================================================
namespace Nelson::Geometry {
//=============================================================================
struct QhullInput
{
    size_t pointCount { 0 };
    size_t dimension { 0 };
    std::vector<double> coordinates;
};
//=============================================================================
struct FacetResult
{
    std::vector<std::vector<int>> facets;
    double area { 0. };
    double volume { 0. };
};
//=============================================================================
struct VoronoiResult
{
    std::vector<double> vertices;
    size_t vertexCount { 0 };
    size_t dimension { 0 };
    std::vector<std::vector<int>> cells;
};
//=============================================================================
NLSGEOMETRY_IMPEXP bool
qhullInputIsDegenerate(const QhullInput& input);
//=============================================================================
NLSGEOMETRY_IMPEXP FacetResult
qhullConvexHull(const QhullInput& input, const std::string& options);
//=============================================================================
NLSGEOMETRY_IMPEXP FacetResult
qhullDelaunay(const QhullInput& input, const std::string& options);
//=============================================================================
NLSGEOMETRY_IMPEXP VoronoiResult
qhullVoronoi(const QhullInput& input, const std::string& options);
//=============================================================================
} // namespace Nelson::Geometry
//=============================================================================
