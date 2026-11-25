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
//=============================================================================
namespace Nelson {
//=============================================================================
class GraphicsObject;
//=============================================================================
NLSGRAPHICS_IMPEXP void
registerCommonProperties(GraphicsObject* go);
//=============================================================================
// Register the base properties shared identically by every standard graphics
// object (CreateFcn, DeleteFcn, BusyAction, Interruptible, BeingDeleted, Type,
// UserData). Subclasses call this instead of repeating the seven registrations.
NLSGRAPHICS_IMPEXP void
registerBaseProperties(GraphicsObject* go);
//=============================================================================
// Register the standard container properties (Parent, Children, Visible, Tag) in
// their default writable form. Only the subclasses that register all four exactly
// this way call this; classes that make one non-writable or omit one keep those
// registrations inline. Registration order is normalized by sortProperties().
NLSGRAPHICS_IMPEXP void
registerStandardContainerProperties(GraphicsObject* go);
//=============================================================================
NLSGRAPHICS_IMPEXP void
initializeCommonProperties(GraphicsObject* go);
//=============================================================================
} // namespace Nelson
//=============================================================================
