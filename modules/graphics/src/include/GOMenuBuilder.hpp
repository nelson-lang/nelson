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
NLSGRAPHICS_IMPEXP go_handle
ContextMenuBuilder(const ArrayOfVector& argIn);
//=============================================================================
NLSGRAPHICS_IMPEXP go_handle
MenuBuilder(const ArrayOfVector& argIn);
//=============================================================================
NLSGRAPHICS_IMPEXP void
setGraphicsMenuParent(go_handle childHandle, go_handle parentHandle);
//=============================================================================
} // namespace Nelson
//=============================================================================
