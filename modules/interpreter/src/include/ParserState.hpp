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
/**
 * These are the different parser states that
 * the parser can be in after a successful parse:
 *    - ScriptBlock corresponds to a parse of a sequence
 *      of statements
 *    - FuncDef corresponds to a parse of a function def
 *    - ParseError corresponds to a syntax error when
 *      parsing.
 */
//=============================================================================
enum ParserState
{
    ScriptBlock,
    FuncDef,
    ParseError
};
//=============================================================================
} // namespace Nelson
//=============================================================================
