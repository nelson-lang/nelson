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
#include <functional>
#include <QtWidgets/QWidget>
#include <QtGui/QPainter>
#include <QtGui/QFontMetrics>
#include <QtGui/QMouseEvent>
#include "UIComponentBindings.hpp"
#include "UIComponentBindingBase.hpp"
#include "GOUISwitch.h"
#include "UIItemsHelpers.h"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "GOArrayOfProperty.hpp"
#include "GOScalarDoubleProperty.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "QStringConverter.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
namespace {
    //=============================================================================
    class QUISwitchWidget : public QWidget
    {
    public:
        bool isOn = false;
        bool isVertical = false;
        std::wstring leftLabel;
        std::wstring rightLabel;
        std::function<void()> toggledCallback;
        //=============================================================================
        QUISwitchWidget(QWidget* parent) : QWidget(parent) { }
        //=============================================================================
        // Width reserved on one side of a horizontal track for a state label,
        // sized to the text so it is never clipped. Both the geometry margins and
        // the paint use it, so the track always lands on the control's Position.
        int
        horizontalLabelMargin(const std::wstring& label) const
        {
            if (label.empty()) {
                return 28;
            }
            QFontMetrics metrics(font());
            return std::max(28, metrics.horizontalAdvance(wstringToQString(label)) + 12);
        }
        //=============================================================================
    protected:
        //=============================================================================
        void
        mouseReleaseEvent(QMouseEvent* /*event*/) override
        {
            isOn = !isOn;
            update();
            if (toggledCallback) {
                toggledCallback();
            }
        }
        //=============================================================================
        void
        paintEvent(QPaintEvent* /*event*/) override
        {
            QPainter painter(this);
            painter.setRenderHint(QPainter::Antialiasing);
            // App-style look: single dark gray rounded track, white round thumb,
            // labels laid outside the track
            QColor trackColor(112, 112, 112);
            QColor knobColor(255, 255, 255);
            QFont labelFont = font();
            painter.setFont(labelFont);
            painter.setPen(QColor(33, 33, 33));
            if (isVertical) {
                painter.drawText(QRect(0, 0, width(), 18), Qt::AlignHCenter | Qt::AlignBottom,
                    wstringToQString(rightLabel));
                painter.drawText(QRect(0, height() - 18, width(), 18),
                    Qt::AlignHCenter | Qt::AlignTop, wstringToQString(leftLabel));
                int trackWidth = 14;
                int x = (width() - trackWidth) / 2;
                QRect track(x, 22, trackWidth, height() - 44);
                painter.setPen(Qt::NoPen);
                painter.setBrush(trackColor);
                painter.drawRoundedRect(track, 7, 7);
                int thumbDiameter = trackWidth + 4;
                int knobY = isOn ? track.top() - 2 : track.bottom() - thumbDiameter + 2;
                painter.setPen(QPen(QColor(150, 150, 150), 1));
                painter.setBrush(knobColor);
                painter.drawEllipse(x - 2, knobY, thumbDiameter, thumbDiameter);
            } else {
                int leftMargin = horizontalLabelMargin(leftLabel);
                int rightMargin = horizontalLabelMargin(rightLabel);
                painter.drawText(QRect(0, 0, leftMargin - 6, height()),
                    Qt::AlignRight | Qt::AlignVCenter, wstringToQString(leftLabel));
                painter.drawText(QRect(width() - rightMargin + 6, 0, rightMargin - 6, height()),
                    Qt::AlignLeft | Qt::AlignVCenter, wstringToQString(rightLabel));
                int trackHeight = 14;
                int y = (height() - trackHeight) / 2;
                QRect track(leftMargin, y, width() - leftMargin - rightMargin, trackHeight);
                painter.setPen(Qt::NoPen);
                painter.setBrush(trackColor);
                painter.drawRoundedRect(track, 7, 7);
                int thumbDiameter = trackHeight + 4;
                int knobX = isOn ? track.right() - thumbDiameter + 2 : track.left() - 2;
                painter.setPen(QPen(QColor(150, 150, 150), 1));
                painter.setBrush(knobColor);
                painter.drawEllipse(knobX, y - 2, thumbDiameter, thumbDiameter);
            }
        }
        //=============================================================================
    };
    //=============================================================================
    class UISwitchBinding : public QtUIComponentBinding
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
            QUISwitchWidget* switchWidget = new QUISwitchWidget(parent);
            switchWidget->toggledCallback = [this]() { onUserToggled(); };
            switchWidget->setAttribute(Qt::WA_TranslucentBackground);
            widget = switchWidget;
        }
        //=============================================================================
        QMargins
        widgetGeometryMargins() override
        {
            // room for the state labels around the track, sized to the text so
            // long labels (e.g. "Hidden" / "Visible") are not clipped
            if (static_cast<GOUISwitch*>(model)->getSwitchStyle() == UISwitchStyle::Slider) {
                QUISwitchWidget* switchWidget = static_cast<QUISwitchWidget*>(widget.data());
                wstringVector items = model->findStringVectorProperty(GO_ITEMS_PROPERTY_NAME_STR);
                std::wstring left = items.size() >= 1 ? items[0] : std::wstring();
                std::wstring right = items.size() >= 2 ? items[1] : std::wstring();
                int leftMargin = switchWidget ? switchWidget->horizontalLabelMargin(left) : 28;
                int rightMargin = switchWidget ? switchWidget->horizontalLabelMargin(right) : 28;
                return QMargins(leftMargin, 0, rightMargin, 0);
            }
            return QMargins(0, 22, 0, 22);
        }
        //=============================================================================
        void
        applyStateToWidget(bool /*isNewWidget*/) override
        {
            QUISwitchWidget* switchWidget = static_cast<QUISwitchWidget*>(widget.data());
            if (!switchWidget) {
                return;
            }
            wstringVector items = model->findStringVectorProperty(GO_ITEMS_PROPERTY_NAME_STR);
            ArrayOf itemsData = model->findArrayOfProperty(GO_ITEMS_DATA_PROPERTY_NAME_STR);
            GOArrayOfProperty* valueProperty
                = static_cast<GOArrayOfProperty*>(model->findProperty(GO_VALUE_PROPERTY_NAME_STR));
            GOScalarProperty* valueIndexProperty = static_cast<GOScalarProperty*>(
                model->findProperty(GO_VALUE_INDEX_PROPERTY_NAME_STR));
            int index = uiItemsIndexOfValue(valueProperty->data(), items, itemsData);
            if (index == 0) {
                if (model->hasChanged(GO_VALUE_PROPERTY_NAME_STR)) {
                    Error(L"Nelson:uicontrol:invalidValueItem",
                        L"'Value' must be an element of 'Items' or 'ItemsData'.");
                }
                index = items.empty() ? 0 : 1;
                valueProperty->data(uiItemsValueAtIndex(index, items, itemsData));
            }
            valueIndexProperty->data((double)index);
            switchWidget->isOn = (index == 2);
            switchWidget->isVertical = model->stringCheck(
                GO_ORIENTATION_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_VERTICAL_STR);
            if (items.size() >= 2) {
                switchWidget->leftLabel = items[0];
                switchWidget->rightLabel = items[1];
            }
            switchWidget->update();
        }
        //=============================================================================
    private:
        //=============================================================================
        void
        onUserToggled()
        {
            if (model->isProgrammaticChange()) {
                return;
            }
            QUISwitchWidget* switchWidget = static_cast<QUISwitchWidget*>(widget.data());
            if (!switchWidget) {
                return;
            }
            wstringVector items = model->findStringVectorProperty(GO_ITEMS_PROPERTY_NAME_STR);
            ArrayOf itemsData = model->findArrayOfProperty(GO_ITEMS_DATA_PROPERTY_NAME_STR);
            GOArrayOfProperty* valueProperty
                = static_cast<GOArrayOfProperty*>(model->findProperty(GO_VALUE_PROPERTY_NAME_STR));
            GOScalarProperty* valueIndexProperty = static_cast<GOScalarProperty*>(
                model->findProperty(GO_VALUE_INDEX_PROPERTY_NAME_STR));
            ArrayOf previousValue = valueProperty->data();
            double previousIndex = valueIndexProperty->data();
            int newIndex = switchWidget->isOn ? 2 : 1;
            ArrayOf newValue = uiItemsValueAtIndex(newIndex, items, itemsData);
            valueProperty->data(newValue);
            valueIndexProperty->data((double)newIndex);
            wstringVector fieldNames
                = { L"Value", L"PreviousValue", L"ValueIndex", L"PreviousValueIndex" };
            ArrayOfVector fieldValues;
            fieldValues << newValue;
            fieldValues << previousValue;
            fieldValues << ArrayOf::doubleScalar((double)newIndex);
            fieldValues << ArrayOf::doubleScalar(previousIndex);
            model->emitCallbackEvent(GO_VALUE_CHANGED_FCN_PROPERTY_NAME_STR, L"ValueChangedData",
                L"ValueChanged", fieldNames, fieldValues);
        }
        //=============================================================================
    };
    //=============================================================================
} // namespace
//=============================================================================
UIComponentBinding*
createUISwitchBinding(GOUIComponent* component)
{
    return new UISwitchBinding(component);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
