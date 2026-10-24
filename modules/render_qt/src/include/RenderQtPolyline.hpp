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
#include <QtCore/QPointF>
#include <QtCore/QVector>
#include <QtGui/QPainter>
//=============================================================================
namespace Nelson {
//=============================================================================
// Strokes a polyline with the painter's current pen, as drawPolyline would.
// Qt's raster engine fills a stroked polyline as one outline: on a long,
// self-intersecting one (a plot of noise) that costs far more than linear
// time, 0.7 s for 2000 points and 5 s for 20000. Long polylines are thus
// stroked in bounded pieces that cap and dash like the whole (see the .cpp).
void
strokePolyline(QPainter* painter, QVector<QPointF> points);
//=============================================================================
} // namespace Nelson
//=============================================================================
