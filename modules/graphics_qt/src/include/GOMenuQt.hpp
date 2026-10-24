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
#include "nlsGraphics_qt_exports.h"
#include "Types.hpp"
//=============================================================================
class QPoint;
class QWidget;
//=============================================================================
namespace Nelson {
class GOWindow;
class GraphicsObject;
//=============================================================================
NLSGRAPHICS_QT_IMPEXP void
rebuildFigureMenus(GOWindow* window);
//=============================================================================
NLSGRAPHICS_QT_IMPEXP bool
popupContextMenu(QWidget* parentWidget, go_handle contextMenuHandle, const QPoint& globalPosition);
//=============================================================================
NLSGRAPHICS_QT_IMPEXP bool
popupContextMenuForObject(
    QWidget* parentWidget, GraphicsObject* object, const QPoint& globalPosition);
//=============================================================================
} // namespace Nelson
//=============================================================================
