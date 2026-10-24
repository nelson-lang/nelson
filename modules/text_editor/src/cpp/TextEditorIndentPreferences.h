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
// Indentation width used by the text editor (Smart Indent and automatic
// indentation while typing). The value is stored in the text editor
// preferences file with the "INDENT_SIZE" key and falls back to
// TEXT_EDITOR_DEFAULT_INDENT_SIZE when it is missing or invalid.
//=============================================================================
#define TEXT_EDITOR_DEFAULT_INDENT_SIZE 2
#define TEXT_EDITOR_MAX_INDENT_SIZE 16
//=============================================================================
int
TextEditorGetIndentSize();
//=============================================================================
void
TextEditorSetIndentSize(int indentSize);
//=============================================================================
