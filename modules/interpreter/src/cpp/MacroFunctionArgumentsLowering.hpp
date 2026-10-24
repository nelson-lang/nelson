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
#include "MacroFunctionDef.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
// Lower the leading "arguments" validation blocks of a function body into
// plain validation statements spliced into its code (prologue and epilogue),
// and apply the varargin signature patch. Idempotent: once lowered, the body
// no longer holds arguments blocks.
void
applyArgumentsBlockLowering(MacroFunctionDef* self);
//=============================================================================
} // namespace Nelson
//=============================================================================
