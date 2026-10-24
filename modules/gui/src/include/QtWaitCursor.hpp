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
#include <QtCore/QtGlobal>
#include <QtWidgets/QApplication>
//=============================================================================
namespace Nelson {
//=============================================================================
inline bool
usesApplicationWaitCursor()
{
#ifdef Q_OS_MACOS
    return false;
#else
    return true;
#endif
}
//=============================================================================
inline void
setApplicationWaitCursor()
{
#ifndef Q_OS_MACOS
    QApplication::setOverrideCursor(Qt::WaitCursor);
#endif
}
//=============================================================================
inline void
restoreApplicationWaitCursor()
{
#ifndef Q_OS_MACOS
    QApplication::restoreOverrideCursor();
#endif
}
//=============================================================================
} // namespace Nelson
//=============================================================================
