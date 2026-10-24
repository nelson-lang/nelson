//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <QtWidgets/QWidget>
#include "UIComponentBindings.hpp"
#include "UIComponentBindingBase.hpp"
#include "GOUIGridLayout.h"
#include "GOPropertyNames.hpp"
#include "GOColorProperty.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
namespace {
    //=============================================================================
    class UIGridLayoutBinding : public QtUIComponentBinding
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
            widget = new QWidget(parent);
        }
        //=============================================================================
        void
        applyStateToWidget(bool /*isNewWidget*/) override
        {
            QWidget* parentWidget = getParentQWidget();
            if (!widget || !parentWidget) {
                return;
            }
            // the grid always fills its parent client area; the cell
            // computation itself is Qt-free and lives in the model
            widget->setGeometry(0, 0, parentWidget->width(), parentWidget->height());
            static_cast<GOUIGridLayout*>(model)->performLayout(
                (double)widget->width(), (double)widget->height());
        }
        //=============================================================================
        void
        applyColorsToWidget() override
        {
            if (!widget) {
                return;
            }
            GOColorProperty* backgroundColor = static_cast<GOColorProperty*>(
                model->findProperty(GO_BACKGROUND_COLOR_PROPERTY_NAME_STR, false));
            if (backgroundColor && !backgroundColor->isNone()) {
                std::vector<double> rgb = backgroundColor->data();
                if (rgb.size() >= 3) {
                    QPalette palette = widget->palette();
                    palette.setColor(QPalette::Window,
                        QColor::fromRgb((int)(rgb[0] * 255.0 + 0.5), (int)(rgb[1] * 255.0 + 0.5),
                            (int)(rgb[2] * 255.0 + 0.5)));
                    widget->setAutoFillBackground(true);
                    widget->setPalette(palette);
                }
            }
        }
        //=============================================================================
    };
    //=============================================================================
} // namespace
//=============================================================================
UIComponentBinding*
createUIGridLayoutBinding(GOUIComponent* component)
{
    return new UIGridLayoutBinding(component);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
