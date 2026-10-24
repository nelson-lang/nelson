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
#include "GraphicsObject.hpp"
#include "nlsGraphics_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
// Limits / MajorTicks / MinorTicks / MajorTickLabels and their *Mode pattern
// shared by uislider, uiknob and uigauge.
//=============================================================================
// Called from updateState(): a user-supplied MajorTicks/MinorTicks/
// MajorTickLabels switches the matching *Mode to 'manual' so the automatic
// tick generation stops overwriting it. Returns true when the auto pass
// (uiTicksUpdateAuto) must run: Limits changed, or ticks changed and the auto
// labels must follow them.
NLSGRAPHICS_IMPEXP bool
uiTicksSyncModes(GraphicsObject* go);
//=============================================================================
// Regenerates the entries still in 'auto' mode from Limits (about
// majorIntervals "nice" major intervals). Auto labels track the current major
// ticks, auto or manual. Auto minor ticks (5 per major interval) only when
// autoMinorTicks.
NLSGRAPHICS_IMPEXP void
uiTicksUpdateAuto(GraphicsObject* go, bool autoMinorTicks, int majorIntervals = 5);
//=============================================================================
// The ticks a dial/scale actually LABELS. When MajorTicks is auto, the widget labels a denser
// "nice" scale sized to the widget (about pixelExtent / divisor intervals) while the MajorTicks
// property keeps its coarser value. When the user set MajorTicks, those are returned as-is.
// outLabels carries the matching labels. divisor tunes the density per widget kind (knob densest).
NLSGRAPHICS_IMPEXP void
uiDisplayMajorTicks(double minimum, double maximum, bool majorAuto,
    const std::vector<double>& majorProperty, const std::vector<std::wstring>& majorLabelProperty,
    double pixelExtent, double divisor, std::vector<double>& outTicks,
    std::vector<std::wstring>& outLabels);
//=============================================================================
} // namespace Nelson
//=============================================================================
