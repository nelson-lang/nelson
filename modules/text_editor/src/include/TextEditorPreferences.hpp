//=============================================================================
// Copyright (c) 2016-2018 Allan CORNET (Nelson)
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
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
#ifdef __APPLE__
#define TEXT_EDITOR_DEFAULT_FONT "Monaco"
#else
#define TEXT_EDITOR_DEFAULT_FONT "Monospace"
#endif
#define TEXT_EDITOR_PREFERENCES_FILENAME "editor.conf"
//=============================================================================
bool
TextEditorSavePreferences(
    QFont currentFont, QPoint pos, QSize sz, Nelson::wstringVector recentFiles);
bool
TextEditorLoadPreferences(
    QFont& currentFont, QPoint& pos, QSize& sz, Nelson::wstringVector& recentFiles);
//=============================================================================
