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
namespace Nelson {
struct LexerContext;
}
//=============================================================================
// Hand-written lexer implementation (defined in NelsonLexerHelpers.cpp).
//
// The flex grammar (NelsonLexer.lxx / generated NelsonLexer.cpp) only owns the
// scanner machinery: its single rule delegates every character to
// nelsonManualLex(). All the actual tokenizing logic lives in the helpers
// translation unit so it is compiled directly and never duplicated into the
// generated file.
//=============================================================================
// Manual lexer entry point invoked by the flex-generated rule action.
int
nelsonManualLex(Nelson::LexerContext& lexerContext);
//=============================================================================
// Flex-generated scanner entry point (defined in NelsonLexer.cpp); called back
// from the helpers (e.g. lexCheckForMoreInput).
int
yylex(Nelson::LexerContext& lexerContext);
//=============================================================================
