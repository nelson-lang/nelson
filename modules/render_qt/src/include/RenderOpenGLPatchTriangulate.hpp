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
#include <array>
#include <cstddef>
#include <vector>
#include "RenderInterface.hpp"
//=============================================================================
// Ear-clips one patch face sub-path (a polygon boundary, possibly non-convex)
// into triangles of source-vertex indices. The polygon is projected onto the
// plane most aligned with the face normal before clipping. Returns false when
// the sub-path is degenerate or cannot be triangulated (the caller then draws a
// triangle fan). Shared by the immediate and vertex-array patch draw paths.
bool
triangulatePatchSubPath(const Face& face, const std::vector<std::size_t>& subPath,
    const point& fallbackNormal, std::vector<std::array<std::size_t, 3>>& triangles);
//=============================================================================
