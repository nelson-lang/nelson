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
// QSettings keys for the text editor user preferences. The gui and text_editor
// modules share only these key names, so neither links to the other.
// Window state (position, size, recent files) stays in the editor's JSON file.
//=============================================================================
#define NELSON_EDITOR_SETTING_INDENT_SIZE "Editor/IndentSize"
#define NELSON_EDITOR_SETTING_CODE_ANALYZER_ENABLED "Editor/CodeAnalyzerEnabled"
#define NELSON_EDITOR_SETTING_FONT_NAME "Editor/FontName"
#define NELSON_EDITOR_SETTING_FONT_SIZE "Editor/FontSize"
#define NELSON_EDITOR_SETTING_FONT_FIXED_PITCH "Editor/FontFixedPitch"
//=============================================================================
#define NELSON_EDITOR_DEFAULT_INDENT_SIZE 2
#define NELSON_EDITOR_MIN_INDENT_SIZE 1
#define NELSON_EDITOR_MAX_INDENT_SIZE 16
//=============================================================================
