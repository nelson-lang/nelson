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
#ifdef _WIN32
#ifdef nlsGraphics_io_qt_EXPORTS
#define NLS_GRAPHICS_IO_QT_EXPORT __declspec(dllexport)
#else
#define NLS_GRAPHICS_IO_QT_EXPORT __declspec(dllimport)
#endif
#else
#define NLS_GRAPHICS_IO_QT_EXPORT __attribute__((visibility("default")))
#endif
//=============================================================================
extern "C" NLS_GRAPHICS_IO_QT_EXPORT void
initializeGraphicsIoQtAdapter();
//=============================================================================
