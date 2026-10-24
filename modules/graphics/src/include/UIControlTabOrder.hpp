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
#include <vector>
#include "nlsGraphics_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
class GOUIControl;
//=============================================================================
// Keyboard (Tab) order of the uicontrols of the figure holding `control`:
// each level in creation order (the reverse of its Children, so uistack and
// a reordered Children change it), a container (uipanel, uibuttongroup, the
// selected uitab) entered where it stands. Hidden controls are left out and,
// with focusableOnly, the ones that cannot take the focus ('text' style,
// Enable 'off' or 'inactive').
NLSGRAPHICS_IMPEXP std::vector<GOUIControl*>
uiControlTabOrder(GOUIControl* control, bool focusableOnly);
//=============================================================================
// The uicontrol Tab (forward) or Shift+Tab gives the focus to from `control`,
// wrapping around; nullptr when no other control can take it.
NLSGRAPHICS_IMPEXP GOUIControl*
nextUIControlInTabOrder(GOUIControl* control, bool forward);
//=============================================================================
} // namespace Nelson
//=============================================================================
