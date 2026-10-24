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
#include "ArrayOf.hpp"
#include "nlsGraphics_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
NLSGRAPHICS_IMPEXP ArrayOf
serializeFiguresToFigPayload(const ArrayOf& figures);
//=============================================================================
NLSGRAPHICS_IMPEXP ArrayOf
restoreFiguresFromFigPayload(const ArrayOf& payload, const std::wstring& filename,
    const std::wstring& visibleAction, bool reuse);
//=============================================================================
} // namespace Nelson
//=============================================================================
