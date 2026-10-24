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
#include "BytecodeChunk.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
// Fuse straight-line scalar arithmetic windows (loads of locals and double
// constants combined with + - * / and unary minus, stored into a local)
// into a single SCALAR_EXPR instruction. Only the first instruction of the
// window is replaced: the remaining original instructions are kept so the
// interpreter can fall back on them when a value is not a real double
// scalar at run time. Jump targets therefore stay valid.
void
fuseScalarExpressions(BytecodeChunk& chunk);
//=============================================================================
} // namespace Nelson
//=============================================================================
