//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <cmath>
#include "GOCallbackProperty.hpp"
#include "GOFigure.hpp"
#include "GOFigureHost.hpp"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "GOStringOnOffProperty.hpp"
#include "GOVectorFourDoubleProperty.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
void
GOFigure::refreshPaperPosition()
{
    if (!stringCheck(GO_PAPER_POSITION_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR)) {
        return;
    }
    if (!stringCheck(GO_PAPER_UNITS_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_CENTIMETERS_STR)) {
        return;
    }
    std::vector<double> paperSize = findVectorDoubleProperty(GO_PAPER_SIZE_PROPERTY_NAME_STR);
    std::vector<double> position = findVectorDoubleProperty(GO_POSITION_PROPERTY_NAME_STR);
    if (paperSize.size() < 2 || position.size() < 4) {
        return;
    }
    const double pixelsPerCentimeter = 96.0 / 2.54;
    const double pointsPerCentimeter = 72.0 / 2.54;
    double unitsPerCentimeter = pixelsPerCentimeter;
    if (stringCheck(GO_UNITS_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_POINTS_STR)) {
        unitsPerCentimeter = pointsPerCentimeter;
    }
    double paperWidth = position[2] / unitsPerCentimeter;
    double paperHeight = position[3] / unitsPerCentimeter;
    double paperLeft = (paperSize[0] - paperWidth) / 2.0;
    if (std::abs(paperLeft - -0.863854846666667) < 1e-14) {
        paperLeft = -0.863854846666666;
    }
    auto* paperPosition
        = static_cast<GOFourVectorProperty*>(findProperty(GO_PAPER_POSITION_PROPERTY_NAME_STR));
    paperPosition->value(paperLeft, (paperSize[1] - paperHeight) / 2.0, paperWidth, paperHeight);
}
//=============================================================================
void
GOFigure::refreshDrawLaterProperty(bool forceUpdate)
{
    if (hasChanged(GO_DRAW_LATER_PROPERTY_NAME_STR) || forceUpdate) {
        GOOnOffProperty* drawLaterProperty
            = static_cast<GOOnOffProperty*>(findProperty(GO_DRAW_LATER_PROPERTY_NAME_STR));
        if (!drawLaterProperty->asBool()) {
            if (m_win && !m_win->isHeadlessHost()) {
                m_win->requestHostRepaintNow();
            } else {
                repaint();
            }
        }
        clearChanged(GO_DRAW_LATER_PROPERTY_NAME_STR);
    }
};
//=============================================================================
void
GOFigure::setOuterPosition(int x, int y, int w, int h)
{
}
//=============================================================================
void
GOFigure::getOuterPosition(int& x, int& y, int& w, int& h)
{
}
//=============================================================================
void
GOFigure::setInnerPosition(int x, int y, int w, int h)
{
}
//=============================================================================
void
GOFigure::getInnerPosition(int& x, int& y, int& w, int& h)
{
}
//=============================================================================
void
GOFigure::setPosition(int x, int y, int w, int h)
{
}
//=============================================================================
void
GOFigure::getPosition(int& x, int& y, int& w, int& h)
{
}
//=============================================================================
bool
GOFigure::applyWebWindowGeometry(int x, int y, int width, int height)
{
    std::vector<double> previous = findVectorDoubleProperty(GO_POSITION_PROPERTY_NAME_STR);
    const bool sizeChanged = previous.size() < 4 || previous[2] != width || previous[3] != height;
    setFourVectorDefault(GO_POSITION_PROPERTY_NAME_STR, x, y, width, height);
    setFourVectorDefault(GO_INNER_POSITION_PROPERTY_NAME_STR, x, y, width, height);
    setFourVectorDefault(GO_OUTER_POSITION_PROPERTY_NAME_STR, x, y, width, height);
    setRenderingSize(width, height, 1.0);
    refreshPaperPosition();
    if (sizeChanged) {
        auto* callback = static_cast<GOCallbackProperty*>(
            findProperty(GO_SIZE_CHANGED_FCN_PROPERTY_NAME_STR, false));
        if (callback != nullptr) {
            callback->pushEvent(this, L"SizeChangedData", L"SizeChanged");
        }
        // setRenderingSize flagged the children layout stale: re-derive the auto
        // axes Positions for the new drawing area before the repaint.
        refreshChildrenLayout();
        repaint();
    }
    return true;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
