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
#include "Types.hpp"
#include <QtCore/QPoint>
#include <QtCore/QSize>
#include <QtGui/QFont>
#include <QtGui/QColor>
//=============================================================================
#define TEXT_EDITOR_DEFAULT_POS_X 0
#define TEXT_EDITOR_DEFAULT_POS_Y 0
#define TEXT_EDITOR_DEFAULT_SIZE_X 640
#define TEXT_EDITOR_DEFAULT_SIZE_Y 480
#define TEXT_EDITOR_PREFERENCES_FILENAME "editor.conf"
#define TEXT_EDITOR_DEFAULT_DEBUG_LINE_COLOR QColor(255, 255, 0, 128) // Semi-transparent yellow
//=============================================================================
bool
TextEditorSavePreferences(QFont currentFont, QPoint pos, QSize sz,
    Nelson::wstringVector recentFiles, QColor debugLineColor);
//=============================================================================
bool
TextEditorLoadPreferences(QFont& currentFont, QPoint& pos, QSize& sz,
    Nelson::wstringVector& recentFiles, QColor& debugLineColor);
//=============================================================================
