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
#include "ArrayOf.hpp"
#include "GeometryQhull.hpp"
#include <string>
#include <vector>
//=============================================================================
namespace Nelson::GeometryGateway {
Geometry::QhullInput
arrayToQhullInput(const ArrayOf& points);
std::string
qhullOptions(const ArrayOfVector& argIn, size_t optionsIndex, const std::string& defaultOptions);
ArrayOf
facetsToArray(const std::vector<std::vector<int>>& facets);
ArrayOf
cellsToArray(const std::vector<std::vector<int>>& cells);
ArrayOf
verticesToArray(const std::vector<double>& vertices, size_t vertexCount, size_t dimension);
//=============================================================================
} // namespace Nelson::GeometryGateway
//=============================================================================
