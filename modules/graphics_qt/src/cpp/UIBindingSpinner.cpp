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
#include <QtGui/QPainter>
#include <QtWidgets/QDoubleSpinBox>
#include <QtWidgets/QStyleOption>
#include "UIComponentBindings.hpp"
#include "UIComponentBindingBase.hpp"
#include "GOUISpinner.h"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "GOArrayOfProperty.hpp"
#include "QStringConverter.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
namespace {
    //=============================================================================
    class QUISpinnerWidget : public QDoubleSpinBox
    {
        GOUISpinner* goSpinner;

    public:
        QUISpinnerWidget(GOUISpinner* owner, QWidget* parent)
            : QDoubleSpinBox(parent), goSpinner(owner)
        {
        }

    protected:
        QString
        textFromValue(double value) const override
        {
            if (goSpinner) {
                return wstringToQString(goSpinner->formatValue(value));
            }
            return QDoubleSpinBox::textFromValue(value);
        }
        //=============================================================================
        // the style sheet styles the up/down buttons, which disables the
        // default arrow primitives: paint small triangles ourselves
        void
        paintEvent(QPaintEvent* event) override
        {
            QDoubleSpinBox::paintEvent(event);
            QStyleOptionSpinBox option;
            initStyleOption(&option);
            QRect upRect
                = style()->subControlRect(QStyle::CC_SpinBox, &option, QStyle::SC_SpinBoxUp, this);
            QRect downRect = style()->subControlRect(
                QStyle::CC_SpinBox, &option, QStyle::SC_SpinBoxDown, this);
            QPainter painter(this);
            painter.setRenderHint(QPainter::Antialiasing);
            painter.setPen(Qt::NoPen);
            painter.setBrush(QColor(0x4c, 0x4c, 0x4c));
            auto drawArrow = [&painter](const QRect& rect, bool pointsUp) {
                if (!rect.isValid()) {
                    return;
                }
                QPointF center = QRectF(rect).center();
                const double halfWidth = 3.5;
                const double halfHeight = 2.5;
                QPolygonF triangle;
                if (pointsUp) {
                    triangle << QPointF(center.x() - halfWidth, center.y() + halfHeight)
                             << QPointF(center.x() + halfWidth, center.y() + halfHeight)
                             << QPointF(center.x(), center.y() - halfHeight);
                } else {
                    triangle << QPointF(center.x() - halfWidth, center.y() - halfHeight)
                             << QPointF(center.x() + halfWidth, center.y() - halfHeight)
                             << QPointF(center.x(), center.y() + halfHeight);
                }
                painter.drawPolygon(triangle);
            };
            drawArrow(upRect, true);
            drawArrow(downRect, false);
        }
    };
    //=============================================================================
    class UISpinnerBinding : public QtUIComponentBinding
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
            QUISpinnerWidget* spinBox
                = new QUISpinnerWidget(static_cast<GOUISpinner*>(model), parent);
            spinBox->setDecimals(10);
            spinBox->setKeyboardTracking(false);
            QObject::connect(spinBox, QOverload<double>::of(&QDoubleSpinBox::valueChanged), spinBox,
                [this](double newValue) { onValueChanged(newValue); });
            widget = spinBox;
        }
        //=============================================================================
        void
        applyStateToWidget(bool isNewWidget) override
        {
            QDoubleSpinBox* spinBox = qobject_cast<QDoubleSpinBox*>(widget.data());
            if (!spinBox) {
                return;
            }
            if (isNewWidget || model->hasChanged(GO_LIMITS_PROPERTY_NAME_STR)) {
                std::vector<double> limits(
                    model->findVectorDoubleProperty(GO_LIMITS_PROPERTY_NAME_STR));
                if (limits.size() == 2) {
                    double lower = std::isinf(limits[0]) ? -1e300 : limits[0];
                    double upper = std::isinf(limits[1]) ? 1e300 : limits[1];
                    spinBox->setRange(lower, upper);
                }
            }
            if (isNewWidget || model->hasChanged(GO_STEP_PROPERTY_NAME_STR)) {
                spinBox->setSingleStep(model->findScalarDoubleProperty(GO_STEP_PROPERTY_NAME_STR));
            }
            if (isNewWidget || model->hasChanged(GO_VALUE_PROPERTY_NAME_STR)
                || model->hasChanged(GO_VALUE_DISPLAY_FORMAT_PROPERTY_NAME_STR)) {
                ArrayOf value = model->findArrayOfProperty(GO_VALUE_PROPERTY_NAME_STR);
                if (!value.isEmpty()) {
                    spinBox->setValue(ArrayOf(value).asDoubleScalar());
                }
            }
            if (isNewWidget || model->hasChanged(GO_EDITABLE_PROPERTY_NAME_STR)) {
                spinBox->setReadOnly(
                    !model->stringCheck(GO_EDITABLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR));
            }
            if (isNewWidget || model->hasChanged(GO_HORIZONTAL_ALIGNMENT_PROPERTY_NAME_STR)) {
                std::wstring horizontalAlignment
                    = model->findStringProperty(GO_HORIZONTAL_ALIGNMENT_PROPERTY_NAME_STR);
                Qt::Alignment alignment = Qt::AlignVCenter;
                if (horizontalAlignment == GO_PROPERTY_VALUE_LEFT_STR) {
                    alignment |= Qt::AlignLeft;
                } else if (horizontalAlignment == GO_PROPERTY_VALUE_CENTER_STR) {
                    alignment |= Qt::AlignHCenter;
                } else {
                    alignment |= Qt::AlignRight;
                }
                spinBox->setAlignment(alignment);
            }
        }
        //=============================================================================
        QString
        additionalStyleSheet() override
        {
            // App-style look: thin border and two stacked bordered buttons on the right;
            // ::up-arrow/::down-arrow are declared with an explicit size so the style
            // keeps drawing the default arrow primitives inside the styled buttons
            // explicit arrow images: the style stops painting the default arrow
            // primitive once ::up-arrow is styled, so reuse the Qt built-in icons
            return QStringLiteral(
                "QDoubleSpinBox { border: 1px solid #8f8f8f; padding-right: 16px; }"
                "QDoubleSpinBox::up-button {"
                " subcontrol-origin: border; subcontrol-position: top right;"
                " width: 16px; background: #fafafa;"
                " border: 1px solid #8f8f8f; border-bottom: none; }"
                "QDoubleSpinBox::down-button {"
                " subcontrol-origin: border; subcontrol-position: bottom right;"
                " width: 16px; background: #fafafa;"
                " border: 1px solid #8f8f8f; }"
                "QDoubleSpinBox::up-button:pressed,"
                "QDoubleSpinBox::down-button:pressed { background: #cfcfcf; }");
        }
        //=============================================================================
    private:
        //=============================================================================
        void
        onValueChanged(double newValue)
        {
            if (model->isProgrammaticChange()) {
                return;
            }
            GOArrayOfProperty* valueProperty
                = static_cast<GOArrayOfProperty*>(model->findProperty(GO_VALUE_PROPERTY_NAME_STR));
            ArrayOf previousValue = valueProperty->data();
            double previousNumeric
                = previousValue.isEmpty() ? 0.0 : ArrayOf(previousValue).asDoubleScalar();
            if (previousNumeric == newValue) {
                return;
            }
            if (model->stringCheck(
                    GO_ROUND_FRACTIONAL_VALUES_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR)) {
                newValue = std::round(newValue);
            }
            valueProperty->data(ArrayOf::doubleScalar(newValue));

            wstringVector fieldNames = { L"Value", L"PreviousValue" };
            ArrayOfVector fieldValues;
            fieldValues << ArrayOf::doubleScalar(newValue);
            fieldValues << ArrayOf::doubleScalar(previousNumeric);
            model->emitCallbackEvent(GO_VALUE_CHANGED_FCN_PROPERTY_NAME_STR, L"ValueChangedData",
                L"ValueChanged", fieldNames, fieldValues);
        }
        //=============================================================================
    };
    //=============================================================================
} // namespace
//=============================================================================
UIComponentBinding*
createUISpinnerBinding(GOUIComponent* component)
{
    return new UISpinnerBinding(component);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
