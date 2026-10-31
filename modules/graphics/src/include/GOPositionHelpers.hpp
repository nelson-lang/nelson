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
#include "nlsGraphics_exports.h"
#include <string>
#include <vector>
//=============================================================================
namespace Nelson {
//=============================================================================
class GraphicsObject;
//=============================================================================
NLSGRAPHICS_IMPEXP bool
syncSimplePositionTriplet(GraphicsObject* object);
NLSGRAPHICS_IMPEXP bool
syncInnerDrivenPositionTriplet(GraphicsObject* object);
NLSGRAPHICS_IMPEXP bool
syncOuterDrivenPositionTriplet(GraphicsObject* object);
NLSGRAPHICS_IMPEXP bool
setPositionConstraintIfPresent(GraphicsObject* object, const std::wstring& value);
NLSGRAPHICS_IMPEXP bool
syncPositionUnitsTriplet(GraphicsObject* object, const std::wstring& previousUnits,
    const std::wstring& currentUnits, double width, double height, bool pixelOrigin = false);
// Position set in the same call as new Units: moves its pixel origin to those Units.
NLSGRAPHICS_IMPEXP void
reconcilePositionPixelOrigin(GraphicsObject* object);
// Convert a chart Position triplet on a Units change; clearPositionChanged drops stale flags.
NLSGRAPHICS_IMPEXP void
syncChartPositionUnits(GraphicsObject* object, bool clearPositionChanged = true);
//=============================================================================
} // namespace Nelson
//=============================================================================
