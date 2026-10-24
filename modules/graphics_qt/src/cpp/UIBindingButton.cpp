//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <QtWidgets/QPushButton>
#include <QtWidgets/QStyle>
#include <QtGui/QImage>
#include <QtGui/QPixmap>
#include <QtGui/QResizeEvent>
#include "UIComponentBindings.hpp"
#include "UIComponentBindingBase.hpp"
#include "GOUIButton.h"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "GOScalarLogicalProperty.hpp"
#include "QStringConverter.hpp"
#include "UIAlignedPushButton.h"
//=============================================================================
namespace Nelson {
//=============================================================================
namespace {
    //=============================================================================
    class UIButtonWidget : public UIAlignedPushButton
    {
    public:
        explicit UIButtonWidget(QWidget* parent) : UIAlignedPushButton(parent) { }

        void
        setModelIcon(const QIcon& icon)
        {
            sourceIconSize
                = icon.availableSizes().isEmpty() ? QSize() : icon.availableSizes().last();
            setIcon(icon);
            updateIconSize();
        }

    protected:
        void
        resizeEvent(QResizeEvent* event) override
        {
            UIAlignedPushButton::resizeEvent(event);
            updateIconSize();
        }

    private:
        void
        updateIconSize()
        {
            if (!sourceIconSize.isValid()) {
                return;
            }
            if (!text().isEmpty()) {
                setIconSize(QSize(style()->pixelMetric(QStyle::PM_SmallIconSize),
                    style()->pixelMetric(QStyle::PM_SmallIconSize)));
                return;
            }
            QSize available(qMax(1, width() - 8), qMax(1, height() - 8));
            QSize displayed = sourceIconSize;
            displayed.scale(available, Qt::KeepAspectRatio);
            setIconSize(displayed);
        }

        QSize sourceIconSize;
    };
    //=============================================================================
    QIcon
    iconFromProperty(QWidget* widget, const ArrayOf& iconValue)
    {
        if (iconValue.isRowVectorCharacterArray() || iconValue.isScalarStringArray()) {
            std::wstring iconName = iconValue.asWideString();
            if (iconName.empty()) {
                return {};
            }
            QStyle* style = widget->style();
            if (iconName == L"question") {
                return style->standardIcon(QStyle::SP_MessageBoxQuestion);
            }
            if (iconName == L"info") {
                return style->standardIcon(QStyle::SP_MessageBoxInformation);
            }
            if (iconName == L"warning") {
                return style->standardIcon(QStyle::SP_MessageBoxWarning);
            }
            if (iconName == L"error") {
                return style->standardIcon(QStyle::SP_MessageBoxCritical);
            }
            if (iconName == L"success") {
                return style->standardIcon(QStyle::SP_DialogApplyButton);
            }
            QPixmap pixmap(wstringToQString(iconName));
            if (!pixmap.isNull()) {
                return QIcon(pixmap);
            }
            return {};
        }
        if (iconValue.isNumeric() && !iconValue.isEmpty() && iconValue.dimensions().length() == 3) {
            if (iconValue.dimensionLength(2) == 3) {
                ArrayOf asDouble(iconValue);
                asDouble.promoteType(NLS_DOUBLE);
                const double* values = (const double*)asDouble.dataPointer();
                int rows = (int)iconValue.rows();
                int cols = (int)iconValue.columns();
                QImage image(cols, rows, QImage::Format_RGB32);
                for (int r = 0; r < rows; ++r) {
                    for (int c = 0; c < cols; ++c) {
                        double red = values[r + c * rows];
                        double green = values[r + c * rows + rows * cols];
                        double blue = values[r + c * rows + 2 * rows * cols];
                        image.setPixel(c, r,
                            qRgb((int)(red * 255.0 + 0.5), (int)(green * 255.0 + 0.5),
                                (int)(blue * 255.0 + 0.5)));
                    }
                }
                return QIcon(QPixmap::fromImage(image));
            }
        }
        return {};
    }
    //=============================================================================
    class UIButtonBinding : public QtUIComponentBinding
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
            QPushButton* button = new UIButtonWidget(parent);
            button->setCheckable(static_cast<GOUIButton*>(model)->isStateButton());
            QObject::connect(button, &QPushButton::clicked, button, [this]() { onClicked(); });
            widget = button;
        }
        //=============================================================================
        void
        applyStateToWidget(bool isNewWidget) override
        {
            QPushButton* button = qobject_cast<QPushButton*>(widget.data());
            if (!button) {
                return;
            }
            if (isNewWidget || model->hasChanged(GO_TEXT_PROPERTY_NAME_STR)) {
                wstringVector lines = model->findStringVectorProperty(GO_TEXT_PROPERTY_NAME_STR);
                std::wstring text;
                for (size_t k = 0; k < lines.size(); ++k) {
                    if (k > 0) {
                        text += L"\n";
                    }
                    text += lines[k];
                }
                button->setText(wstringToQString(text));
            }
            if (isNewWidget || model->hasChanged(GO_ICON_PROPERTY_NAME_STR)) {
                ArrayOf iconValue = model->findArrayOfProperty(GO_ICON_PROPERTY_NAME_STR);
                static_cast<UIButtonWidget*>(button)->setModelIcon(
                    iconFromProperty(button, iconValue));
            }
            if (static_cast<GOUIButton*>(model)->isStateButton()
                && (isNewWidget || model->hasChanged(GO_VALUE_PROPERTY_NAME_STR))) {
                GOScalarLogicalProperty* value = static_cast<GOScalarLogicalProperty*>(
                    model->findProperty(GO_VALUE_PROPERTY_NAME_STR));
                button->setChecked(value->data());
            }
            if (isNewWidget || model->hasChanged(GO_HORIZONTAL_ALIGNMENT_PROPERTY_NAME_STR)) {
                applyColorsToWidget();
            }
            if (isNewWidget || model->hasChanged(GO_VERTICAL_ALIGNMENT_PROPERTY_NAME_STR)) {
                static_cast<UIButtonWidget*>(button)->setVerticalAlignment(
                    model->findStringProperty(GO_VERTICAL_ALIGNMENT_PROPERTY_NAME_STR));
            }
        }
        //=============================================================================
        QString
        additionalStyleSheet() override
        {
            // App-style button chrome: thin gray border, rounded corners,
            // darker face when a state button is pressed
            QString styleSheet
                = QStringLiteral("QPushButton { border: 1px solid #8f8f8f; border-radius: 3px; }"
                                 "QPushButton:pressed { background-color: #cfcfcf; }"
                                 "QPushButton:checked { background-color: #cfcfcf; }");
            std::wstring horizontalAlignment
                = model->findStringProperty(GO_HORIZONTAL_ALIGNMENT_PROPERTY_NAME_STR);
            if (horizontalAlignment == GO_PROPERTY_VALUE_LEFT_STR) {
                styleSheet += QStringLiteral("QPushButton { text-align: left; }");
            } else if (horizontalAlignment == GO_PROPERTY_VALUE_RIGHT_STR) {
                styleSheet += QStringLiteral("QPushButton { text-align: right; }");
            }
            return styleSheet;
        }
        //=============================================================================
    private:
        //=============================================================================
        void
        onClicked()
        {
            if (model->isProgrammaticChange()) {
                return;
            }
            if (static_cast<GOUIButton*>(model)->isStateButton()) {
                QPushButton* button = qobject_cast<QPushButton*>(widget.data());
                if (!button) {
                    return;
                }
                GOScalarLogicalProperty* value = static_cast<GOScalarLogicalProperty*>(
                    model->findProperty(GO_VALUE_PROPERTY_NAME_STR));
                bool previousValue = value->data();
                bool newValue = button->isChecked();
                value->data(newValue);
                wstringVector fieldNames = { L"Value", L"PreviousValue" };
                ArrayOfVector fieldValues;
                fieldValues << ArrayOf::logicalScalar(newValue);
                fieldValues << ArrayOf::logicalScalar(previousValue);
                model->emitCallbackEvent(GO_VALUE_CHANGED_FCN_PROPERTY_NAME_STR,
                    L"ValueChangedData", L"ValueChanged", fieldNames, fieldValues);
            } else {
                model->emitCallbackEvent(
                    GO_BUTTON_PUSHED_FCN_PROPERTY_NAME_STR, L"ButtonPushedData", L"ButtonPushed");
            }
        }
        //=============================================================================
    };
    //=============================================================================
} // namespace
//=============================================================================
UIComponentBinding*
createUIButtonBinding(GOUIComponent* component)
{
    return new UIButtonBinding(component);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
