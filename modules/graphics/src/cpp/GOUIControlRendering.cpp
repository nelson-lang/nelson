//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <QtGui/QImage>
#include <QtGui/QPainter>
#include "GOUIControl.h"
#include "GOPropertyNames.hpp"
#include "GOUnitsProperty.hpp"
#include "UnitsConversionHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
void
GOUIControl::paintMe(RenderInterface& gc)
{
    if (widget) {
        if (gc.getRenderName() != L"GL") {
            QImage image(widget->size(), QImage::Format_ARGB32);
            QPainter painter(&image);
            widget->render(&painter);
            GOUnitsProperty* unitsProperty
                = (GOUnitsProperty*)findProperty(GO_UNITS_PROPERTY_NAME_STR);
            std::wstring currentUnits = unitsProperty->data();
            std::vector<double> sizevec(findVectorDoubleProperty(GO_POSITION_PROPERTY_NAME_STR));
            if (currentUnits != GO_PROPERTY_VALUE_PIXELS_STR) {
                auto [pixelX, pixelY] = convertToPixels(
                    currentUnits, sizevec[0], sizevec[1], gc.width(), gc.height());
                auto [pixelW, pixelH] = convertToPixels(
                    currentUnits, sizevec[2], sizevec[3], gc.width(), gc.height());
                sizevec[0] = pixelX;
                sizevec[1] = pixelY;
                sizevec[2] = pixelW;
                sizevec[3] = pixelH;
            }
            QPoint pt(sizevec[0], sizevec[1]);
            int y = gc.height() - sizevec[1] - widget->height();
            gc.drawImage(sizevec[0], y, image);
        }
    }
}
//=============================================================================
QPoint
GOUIControl::convertToBottomLeft(const QPoint& topLeftPos)
{
    int x = topLeftPos.x();
    int y = getParentWidget()->height() - topLeftPos.y() - widget->height();
    return QPoint(x, y);
}
//=============================================================================
void
GOUIControl::show()
{
    if (widget) {
        widget->show();
    }
}
//=============================================================================
void
GOUIControl::hide()
{
    if (widget) {
        widget->hide();
    }
}
//=============================================================================
void
GOUIControl::setFocus()
{
    if (widget) {
        widget->raise();
        widget->setFocus();
    }
}
//=============================================================================
}
//=============================================================================
