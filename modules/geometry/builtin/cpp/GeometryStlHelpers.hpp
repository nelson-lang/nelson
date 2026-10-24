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
#include <array>
#include <string>
#include <vector>
//=============================================================================
namespace Nelson::GeometryGateway::Stl {
//=============================================================================
struct Mesh
{
    std::vector<std::array<double, 3>> points;
    std::vector<std::array<indexType, 3>> connectivity;
    std::vector<uint16> attributes;
    std::vector<double> solidIds;
    bool binary = false;
};
//=============================================================================
Mesh
readFile(const std::wstring& filename);
//=============================================================================
void
writeFile(const std::wstring& filename, const std::string& fileFormat,
    const std::vector<std::array<double, 3>>& points,
    const std::vector<std::array<indexType, 3>>& connectivity,
    const std::vector<uint16>& attributes, const std::vector<double>& solidIds);
//=============================================================================
ArrayOf
connectivityToArray(const std::vector<std::array<indexType, 3>>& connectivity);
//=============================================================================
ArrayOf
pointsToArray(const std::vector<std::array<double, 3>>& points);
//=============================================================================
ArrayOf
attributesToArray(const std::vector<uint16>& attributes, bool binary, indexType rows);
//=============================================================================
ArrayOf
solidIdsToArray(const std::vector<double>& solidIds);
//=============================================================================
std::vector<std::array<indexType, 3>>
arrayToConnectivity(const ArrayOf& value, indexType pointCount);
//=============================================================================
std::vector<std::array<double, 3>>
arrayToPoints(const ArrayOf& value);
//=============================================================================
std::vector<uint16>
arrayToAttributes(const ArrayOf& value, indexType triangleCount);
//=============================================================================
std::vector<double>
arrayToSolidIds(const ArrayOf& value, indexType triangleCount);
//=============================================================================
std::wstring
textArgument(const ArrayOf& value, const std::wstring& message);
//=============================================================================
} // namespace Nelson::GeometryGateway::Stl
//=============================================================================
