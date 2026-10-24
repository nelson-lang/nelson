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
// Items / ItemsData / Value / ValueIndex pattern shared by uidropdown,
// uilistbox, uidiscreteknob and uiswitch:
//   ItemsData empty      -> Value is an element of Items (text)
//   ItemsData non empty  -> Value is the matching element of ItemsData
//=============================================================================
// 1-based index of value; 0 when not found
NLSGRAPHICS_IMPEXP int
uiItemsIndexOfValue(const ArrayOf& value, const wstringVector& items, const ArrayOf& itemsData);
//=============================================================================
// value shown for a given 1-based index (Items text or ItemsData element)
NLSGRAPHICS_IMPEXP ArrayOf
uiItemsValueAtIndex(int index, const wstringVector& items, const ArrayOf& itemsData);
//=============================================================================
} // namespace Nelson
//=============================================================================
