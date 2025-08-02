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
#include "Interface.hpp"
#include "nlsDisplay_format_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
NLSDISPLAY_FORMAT_IMPEXP void
DisplaySingle(
    size_t evaluatorID, Interface* io, const ArrayOf& A, const std::wstring& name, bool asDisp);
//=============================================================================
} // namespace Nelson
//=============================================================================
