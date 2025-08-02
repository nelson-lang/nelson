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
//=============================================================================
#define TEXT_EDITOR_DEFAULT_POS_X 0
#define TEXT_EDITOR_DEFAULT_POS_Y 0
#define TEXT_EDITOR_DEFAULT_SIZE_X 640
#define TEXT_EDITOR_DEFAULT_SIZE_Y 480
#define TEXT_EDITOR_PREFERENCES_FILENAME "editor.conf"
//=============================================================================
bool
TextEditorSavePreferences(
    QFont currentFont, QPoint pos, QSize sz, Nelson::wstringVector recentFiles);
//=============================================================================
bool
TextEditorLoadPreferences(
    QFont& currentFont, QPoint& pos, QSize& sz, Nelson::wstringVector& recentFiles);
//=============================================================================
