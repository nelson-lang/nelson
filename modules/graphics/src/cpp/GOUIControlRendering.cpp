//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <QtWidgets/QPushButton>
#include <QtWidgets/QRadioButton>
#include <QtWidgets/QCheckBox>
#include <QtWidgets/QSlider>
#include <QtWidgets/QLabel>
#include <QtWidgets/QLineEdit>
#include <QtWidgets/QComboBox>
#include <QtWidgets/QListWidget>
#include <QtGui/QImage>
#include <QtGui/QPainter>
#include <QtGui/QPixmap>
#include "QMultiLineEdit.h"
#include "QStringConverter.hpp"
#include "GOUIControl.h"
#include "GOPropertyValues.hpp"
#include "GOPropertyNames.hpp"
#include "GOColorProperty.hpp"
#include "GOStringProperty.hpp"
#include "GOStringVectorProperty.hpp"
#include "GOArrayOfProperty.hpp"
#include "GOGObjectsProperty.hpp"
#include "GOScalarDoubleProperty.hpp"
#include "GOFontAngleProperty.hpp"
#include "GOFontUnitsProperty.hpp"
#include "GOFontWeightProperty.hpp"
#include "GOStringOnOffProperty.hpp"
#include "GOAlignHorizProperty.hpp"
#include "GOVectorFourDoubleProperty.hpp"
#include "GOUnitsProperty.hpp"
#include "GOVectorProperty.hpp"
#include "GOControlStyleProperty.hpp"
#include "GOCallbackProperty.hpp"
#include "AnonymousMacroFunctionDef.hpp"
#include "GOHelpers.hpp"
#include "Error.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
void
GOUIControl::paintMe(RenderInterface& gc)
{
    if (widget && gc.getRenderName() != L"GL") {
        QImage image(widget->size(), QImage::Format_ARGB32);
        QPainter painter(&image);
        widget->render(&painter);
        std::vector<double> sizevec(findVectorDoubleProperty(GO_POSITION_PROPERTY_NAME_STR));
        gc.drawImage(sizevec[0], sizevec[1], image);
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
}
//=============================================================================
