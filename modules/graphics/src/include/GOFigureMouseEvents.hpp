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
#include <string>
#include "Types.hpp"
#include "nlsGraphics_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
class GOFigure;
class GraphicsObject;
//=============================================================================
// x, y are logical pixels of the figure drawing area, origin at its bottom-left corner.
// Writes the figure CurrentPoint (in the figure Units) and the CurrentPoint of every
// axes, in data coordinates, including axes inside uipanel / uitab / tiledlayout
// containers. Axes points are computed on the surface the axes are painted on.
NLSGRAPHICS_IMPEXP void
updateFigureCurrentPoint(GOFigure* fig, double x, double y);
//=============================================================================
// Mouse event for backends without a native window (web, headless). action is L"press",
// L"motion" or L"release". Updates CurrentPoint, then queues the callbacks like the Qt
// window does. Returns true when the figure listens to pointer motion, so the caller can
// skip motion events otherwise. selectionType, when not empty, is written to SelectionType
// on a L"press" before the callbacks are queued.
NLSGRAPHICS_IMPEXP bool
dispatchFigureMouseEvent(GOFigure* fig, const std::wstring& action, double x, double y,
    const std::wstring& selectionType = L"");
//=============================================================================
// button is 0 (left), 1 (middle) or 2 (right). A plain left click is L"normal",
// shift-left or middle is L"extend", ctrl-left or right is L"alt", a double click is L"open".
NLSGRAPHICS_IMPEXP std::wstring
resolveSelectionType(int button, bool shiftDown, bool ctrlDown, bool doubleClick);
//=============================================================================
// An empty value leaves SelectionType unchanged.
NLSGRAPHICS_IMPEXP void
setFigureSelectionType(GOFigure* fig, const std::wstring& selectionType);
//=============================================================================
// Topmost pickable object under the pixel (x, y), in the coordinates of
// updateFigureCurrentPoint, or -1 when none. Geometry is projected as painted and tested
// in pixels. Front-most wins, ties go to the last drawn. Visible == 'off', HitTest == 'off'
// and PickableParts == 'none' objects are skipped.
NLSGRAPHICS_IMPEXP int64
pickObjectAtPixel(GOFigure* fig, double x, double y);
//=============================================================================
// Colorbar or legend under the pixel (x, y), in the figure or in its ui containers, or -1.
NLSGRAPHICS_IMPEXP int64
decorationAtPixel(GOFigure* fig, double x, double y);
//=============================================================================
// Fires the ItemHitFcn of the legend entry under the pixel (x, y), if any.
NLSGRAPHICS_IMPEXP void
fireLegendItemHit(GOFigure* fig, double x, double y, const std::wstring& selectionType);
//=============================================================================
// True when WindowButtonMotionFcn is set. Motion only updates CurrentPoint and runs
// callbacks in that case.
NLSGRAPHICS_IMPEXP bool
figureListensToMotion(GOFigure* fig);
// A ui container that is not on screen (Visible off, or a uitab that is not selected).
NLSGRAPHICS_IMPEXP bool
isHiddenUIContainer(GraphicsObject* object);
//=============================================================================
} // namespace Nelson
//=============================================================================
