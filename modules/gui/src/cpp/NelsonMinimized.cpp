//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "NelsonMinimized.hpp"
#include "NelSon_engine_mode.h"
#include "QtMainWindow.h"
#include "NelsonConfiguration.hpp"
//===================================================================================
bool
setNelsonMinimized(bool minimize)
{
    if (NELSON_ENGINE_MODE::GUI == Nelson::NelsonConfiguration::getInstance()->getNelsonEngineMode()
        && Nelson::NelsonConfiguration::getInstance()->getMainGuiObject()) {
        QtMainWindow* NelSonQtMainWindow
            = (QtMainWindow*)Nelson::NelsonConfiguration::getInstance()->getMainGuiObject();
        if (minimize) {
            NelSonQtMainWindow->setWindowState(Qt::WindowMinimized);
        } else {
            NelSonQtMainWindow->setWindowState(Qt::WindowNoState);
        }
        NelSonQtMainWindow->setVisible(true);
        return true;
    }
    return false;
}
//===================================================================================
bool
getNelsonMinimized()
{
    if (NELSON_ENGINE_MODE::GUI == Nelson::NelsonConfiguration::getInstance()->getNelsonEngineMode()
        && Nelson::NelsonConfiguration::getInstance()->getMainGuiObject()) {
        QtMainWindow* NelSonQtMainWindow
            = (QtMainWindow*)Nelson::NelsonConfiguration::getInstance()->getMainGuiObject();
        return NelSonQtMainWindow->windowState() == Qt::WindowMinimized
            && NelSonQtMainWindow->isVisible();
    }
    return false;
}
//===================================================================================
