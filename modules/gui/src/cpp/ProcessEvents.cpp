//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <QtCore/QElapsedTimer>
#include <QtWidgets/QApplication>
#include "ProcessEvents.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static QElapsedTimer timerLoopEvents;
static bool doOnce = true;
//=============================================================================
#define WAIT_20_MS 20
//=============================================================================
void
ProcessEvents(bool bWaitEvents)
{
    if (doOnce) {
        doOnce = false;
        timerLoopEvents.start();
    }
    if (timerLoopEvents.elapsed() > WAIT_20_MS) {
        timerLoopEvents.restart();
        if (bWaitEvents) {
            qApp->processEvents(QEventLoop::WaitForMoreEvents);
        } else {
            qApp->processEvents(QEventLoop::AllEvents);
        }
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
void
NelSonProcessEvents(bool bWaitEvents)
{
    Nelson::ProcessEvents(bWaitEvents);
}
//=============================================================================
