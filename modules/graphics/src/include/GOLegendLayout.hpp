//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include <utility>
#include "GraphicsObject.hpp"
#include "Types.hpp"
#include "nlsGraphics_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
bool NLSGRAPHICS_IMPEXP
applyLegendLayoutTile(GraphicsObject* object, int64 peerAxesHandle,
    const std::pair<double, double>& preferredSizeInPixels);
//=============================================================================
} // namespace Nelson
//=============================================================================
