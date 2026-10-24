//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <QtWidgets/QStyleOptionButton>
#include <QtWidgets/QStylePainter>
#include "UIAlignedPushButton.h"
#include "GOPropertyValues.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
// room kept between the label band and the button border
static const int LABEL_MARGIN = 4;
//=============================================================================
static QRect
labelBand(const QWidget* button, const QStyleOptionButton& option, Qt::Alignment alignment);
//=============================================================================
UIAlignedPushButton::UIAlignedPushButton(QWidget* parent) : QPushButton(parent) { }
//=============================================================================
void
UIAlignedPushButton::setVerticalAlignment(const std::wstring& alignment)
{
    Qt::Alignment newAlignment = Qt::AlignVCenter;
    if (alignment == GO_PROPERTY_VALUE_TOP_STR) {
        newAlignment = Qt::AlignTop;
    } else if (alignment == GO_PROPERTY_VALUE_BOTTOM_STR) {
        newAlignment = Qt::AlignBottom;
    }
    if (newAlignment != verticalAlignment) {
        verticalAlignment = newAlignment;
        update();
    }
}
//=============================================================================
void
UIAlignedPushButton::paintEvent(QPaintEvent* event)
{
    if (verticalAlignment == Qt::AlignVCenter) {
        QPushButton::paintEvent(event);
        return;
    }
    QStylePainter painter(this);
    QStyleOptionButton option;
    initStyleOption(&option);
    painter.drawControl(QStyle::CE_PushButtonBevel, option);
    // the style centers the label vertically in the rect it is given
    option.rect = labelBand(this, option, verticalAlignment);
    painter.drawControl(QStyle::CE_PushButtonLabel, option);
}
//=============================================================================
QRect
labelBand(const QWidget* button, const QStyleOptionButton& option, Qt::Alignment alignment)
{
    QRect contents
        = button->style()->subElementRect(QStyle::SE_PushButtonContents, &option, button);
    int labelHeight
        = button->fontMetrics().boundingRect(QRect(), Qt::TextShowMnemonic, option.text).height();
    if (!option.icon.isNull()) {
        labelHeight = qMax(labelHeight, option.iconSize.height());
    }
    // the border and padding the style puts around its contents stay around the band
    int chrome = option.rect.height() - contents.height();
    int bandHeight = qMin(option.rect.height(), labelHeight + chrome + 2 * LABEL_MARGIN);
    QRect band = option.rect;
    if (alignment == Qt::AlignTop) {
        band.setHeight(bandHeight);
    } else {
        band.setTop(band.bottom() - bandHeight + 1);
    }
    return band;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
