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
#include "Types.hpp"
#include "nlsGraphics_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
class GraphicsObject;
//=============================================================================
// Parent rules of the UI components, shared by their builders and by
// set(h, 'Parent', p).
//=============================================================================
// parent constraint of a UI component
enum class UIParentKind
{
    AnyContainer, // figure, panel, tab, button group, grid layout
    TabGroupOnly, // uitab (an implicit tab group is created when needed)
    ButtonGroupOnly // uiradiobutton / uitogglebutton
};
//=============================================================================
struct ResolvedUIParent
{
    int64 figureID = -1;
    int64 parentHandle = -1;
    bool isFigure = true;
};
//=============================================================================
// figure or UI container attached to a figure, else raises the builder error
ResolvedUIParent
resolveUIParent(const ArrayOf& parentValue);
//=============================================================================
bool
parentIsTabGroup(const ResolvedUIParent& parent);
//=============================================================================
// raises the builder error when parent does not match kind
void
checkParentKind(UIParentKind kind, const ResolvedUIParent& parent);
//=============================================================================
// Parent rule of set(h, 'Parent', p) on a UI component, a tree node or a
// uicontrol: the one its builder applies. Raises the builder error, before
// anything changes, when the new parent is refused. Other objects pass.
NLSGRAPHICS_IMPEXP void
checkUIParentChange(GraphicsObject* child, const ArrayOf& newParent);
//=============================================================================
} // namespace Nelson
//=============================================================================
