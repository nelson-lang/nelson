//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <algorithm>
#include <QtWidgets/QWidget>
#include <QtGui/QPainter>
#include "UIComponentBindings.hpp"
#include "UIComponentBindingBase.hpp"
#include "GOUILamp.h"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "GOColorProperty.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
namespace {
    //=============================================================================
    class QUILampWidget : public QWidget
    {
    public:
        QColor color { 0, 255, 0 };
        QUILampWidget(QWidget* parent) : QWidget(parent)
        {
            // uilamp has no BackgroundColor: only the disc is painted, the
            // corners show the parent (also in getframe snapshots).
            setAutoFillBackground(false);
            setAttribute(Qt::WA_TranslucentBackground);
        }

    protected:
        void
        paintEvent(QPaintEvent* /*event*/) override
        {
            QPainter painter(this);
            painter.setRenderHint(QPainter::Antialiasing);
            int diameter = std::min(width(), height()) - 2;
            painter.setPen(QPen(QColor(120, 120, 120), 1));
            painter.setBrush(color);
            painter.drawEllipse(1, 1, diameter, diameter);
        }
    };
    //=============================================================================
    class UILampBinding : public QtUIComponentBinding
    {
    public:
        using QtUIComponentBinding::QtUIComponentBinding;
        //=============================================================================
    protected:
        //=============================================================================
        void
        createWidget() override
        {
            QWidget* parent = getParentQWidget();
            if (!parent) {
                return;
            }
            widget = new QUILampWidget(parent);
        }
        //=============================================================================
        void
        applyStateToWidget(bool /*isNewWidget*/) override
        {
            QUILampWidget* lamp = static_cast<QUILampWidget*>(widget.data());
            if (!lamp) {
                return;
            }
            GOColorProperty* color = static_cast<GOColorProperty*>(
                model->findProperty(GO_COLOR_PROPERTY_NAME_STR, false));
            if (color && !color->isNone()) {
                std::vector<double> rgb = color->data();
                if (rgb.size() >= 3) {
                    lamp->color = QColor::fromRgb((int)(rgb[0] * 255.0 + 0.5),
                        (int)(rgb[1] * 255.0 + 0.5), (int)(rgb[2] * 255.0 + 0.5));
                }
            }
            lamp->update();
        }
        //=============================================================================
    };
    //=============================================================================
} // namespace
//=============================================================================
UIComponentBinding*
createUILampBinding(GOUIComponent* component)
{
    return new UILampBinding(component);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
