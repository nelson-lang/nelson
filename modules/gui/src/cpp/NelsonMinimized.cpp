//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// Alternatively, you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of
// the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this program. If not, see <http://www.gnu.org/licenses/>.
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
