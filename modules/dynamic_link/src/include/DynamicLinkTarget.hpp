//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include "nlsDynamic_link_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
// True when NELSON_DL_TARGET selects a wasm32 cross build: the dynamic-link
// tools then describe Emscripten side modules instead of host libraries
// (see dltarget).
NLSDYNAMIC_LINK_IMPEXP bool
IsWasm32DynamicLinkTarget();
//=============================================================================
} // namespace Nelson
//=============================================================================
