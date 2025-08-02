//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <QtWidgets/QApplication>
#include "DrawNow.hpp"
#include "GOFiguresManager.hpp"
#include "GOFigure.hpp"
#include "GOStringOnOffProperty.hpp"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
void
drawNow()
{
    std::vector<int64> figures = getFigureGraphicsObjects();
    for (auto f : figures) {
        GOWindow* window = getFigure(f);
        if (!window) {
            return;
        }
        GOFigure* goFigure = window->getGOFigure();
        if (!goFigure) {
            return;
        }
        goFigure->setRenderingStateInvalid(true);
        GOOnOffProperty* drawLaterProperty = static_cast<GOOnOffProperty*>(
            goFigure->findProperty(GO_DRAW_LATER_PROPERTY_NAME_STR));
        std::wstring value = drawLaterProperty->data();
        bool restoreOffAfter = false;
        if (value == GO_PROPERTY_VALUE_ON_STR) {
            goFigure->setStringDefault(GO_DRAW_LATER_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR);
            restoreOffAfter = true;
        }
        goFigure->repaint();
        qApp->processEvents(QEventLoop::AllEvents, 50);
        if (restoreOffAfter) {
            goFigure->setStringDefault(GO_DRAW_LATER_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
        }
    }
}
//=============================================================================
}
//=============================================================================
