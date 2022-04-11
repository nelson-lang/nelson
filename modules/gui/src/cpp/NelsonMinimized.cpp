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
#include "GetNelsonMainEvaluatorDynamicFunction.hpp"
#include "Evaluator.hpp"
#include "NelSon_engine_mode.h"
#include "QtMainWindow.h"
//===================================================================================
bool
setNelsonMinimized(bool minimize)
{
    Nelson::Evaluator* eval = (Nelson::Evaluator*)GetNelsonMainEvaluatorDynamicFunction();
    if (NELSON_ENGINE_MODE::GUI == eval->getNelsonEngineMode() && eval->mainGuiObject) {
        QtMainWindow* NelSonQtMainWindow = (QtMainWindow*)eval->mainGuiObject;
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
    Nelson::Evaluator* eval = (Nelson::Evaluator*)GetNelsonMainEvaluatorDynamicFunction();
    if (NELSON_ENGINE_MODE::GUI == eval->getNelsonEngineMode() && eval->mainGuiObject) {
        QtMainWindow* NelSonQtMainWindow = (QtMainWindow*)eval->mainGuiObject;
        return NelSonQtMainWindow->windowState() == Qt::WindowMinimized
            && NelSonQtMainWindow->isVisible();
    }
    return false;
}
//===================================================================================
