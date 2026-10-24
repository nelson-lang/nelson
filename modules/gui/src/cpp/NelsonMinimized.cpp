//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <QtCore/QTimer>
#include "NelsonMinimized.hpp"
#include "NelSon_engine_mode.h"
#include "QtMainWindow.h"
#include "NelsonConfiguration.hpp"
//=============================================================================
bool
setNelsonMinimized(bool minimize)
{
    if (NELSON_ENGINE_MODE::GUI == Nelson::NelsonConfiguration::getInstance()->getNelsonEngineMode()
        && Nelson::NelsonConfiguration::getInstance()->getMainGuiObject()) {
        QtMainWindow* nelsonQtMainWindow = static_cast<QtMainWindow*>(
            Nelson::NelsonConfiguration::getInstance()->getMainGuiObject());
        if (minimize) {
            QTimer::singleShot(0, nelsonQtMainWindow, [nelsonQtMainWindow]() {
                nelsonQtMainWindow->setWindowState(Qt::WindowMinimized);
            });
        } else {

            QTimer::singleShot(0, nelsonQtMainWindow, [nelsonQtMainWindow]() {
                nelsonQtMainWindow->setWindowState(Qt::WindowNoState);
                // Some Ubuntu/Linux systems need a second attempt.
                QTimer::singleShot(100, nelsonQtMainWindow, [nelsonQtMainWindow]() {
                    nelsonQtMainWindow->activateWindow();
                    nelsonQtMainWindow->raise();
                });
            });
        }
        nelsonQtMainWindow->setVisible(true);
        return true;
    }
    return false;
}
//=============================================================================
bool
getNelsonMinimized()
{
    if (NELSON_ENGINE_MODE::GUI == Nelson::NelsonConfiguration::getInstance()->getNelsonEngineMode()
        && Nelson::NelsonConfiguration::getInstance()->getMainGuiObject()) {
        QtMainWindow* nelsonQtMainWindow = static_cast<QtMainWindow*>(
            Nelson::NelsonConfiguration::getInstance()->getMainGuiObject());

        return nelsonQtMainWindow->isMinimized();
    }
    return false;
}
//=============================================================================
