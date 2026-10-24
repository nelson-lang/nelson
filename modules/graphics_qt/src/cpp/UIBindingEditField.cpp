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
#include <cwctype>
#include <stdexcept>
#include <QtWidgets/QLineEdit>
#include "UIComponentBindings.hpp"
#include "UIComponentBindingBase.hpp"
#include "GOUIEditField.h"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "GOArrayOfProperty.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "QStringConverter.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
namespace {
    //=============================================================================
    class UIEditFieldBinding : public QtUIComponentBinding
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
            QLineEdit* lineEdit = new QLineEdit(parent);
            QObject::connect(
                lineEdit, &QLineEdit::editingFinished, lineEdit, [this]() { onEditingFinished(); });
            if (!static_cast<GOUIEditField*>(model)->isNumeric()) {
                QObject::connect(lineEdit, &QLineEdit::textEdited, lineEdit,
                    [this](const QString& text) { onTextEdited(text); });
            }
            widget = lineEdit;
        }
        //=============================================================================
        void
        applyStateToWidget(bool isNewWidget) override
        {
            QLineEdit* lineEdit = qobject_cast<QLineEdit*>(widget.data());
            if (!lineEdit) {
                return;
            }
            GOUIEditField* editField = static_cast<GOUIEditField*>(model);
            bool numericStyle = editField->isNumeric();
            if (isNewWidget || model->hasChanged(GO_VALUE_PROPERTY_NAME_STR)
                || (numericStyle && model->hasChanged(GO_VALUE_DISPLAY_FORMAT_PROPERTY_NAME_STR))) {
                ArrayOf value = model->findArrayOfProperty(GO_VALUE_PROPERTY_NAME_STR);
                if (numericStyle) {
                    if (value.isEmpty()) {
                        lineEdit->setText(QString());
                    } else {
                        if (!value.isNumeric() || !value.isScalar()) {
                            Error(L"Nelson:uicontrol:invalidValue",
                                L"'Value' must be a numeric scalar.");
                        }
                        double numericValue = ArrayOf(value).asDoubleScalar();
                        lineEdit->setText(
                            wstringToQString(editField->formatNumericValue(numericValue)));
                    }
                } else {
                    if (!(value.isRowVectorCharacterArray() || value.isScalarStringArray()
                            || value.isEmpty())) {
                        Error(L"Nelson:uicontrol:invalidValueString",
                            L"'Value' must be a character vector or scalar string.");
                    }
                    std::wstring text = value.isEmpty() ? L"" : ArrayOf(value).asWideString();
                    lineEdit->setText(wstringToQString(text));
                }
            }
            if (isNewWidget || model->hasChanged(GO_EDITABLE_PROPERTY_NAME_STR)) {
                lineEdit->setReadOnly(
                    !model->stringCheck(GO_EDITABLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR));
            }
            if (isNewWidget || model->hasChanged(GO_PLACEHOLDER_PROPERTY_NAME_STR)) {
                lineEdit->setPlaceholderText(
                    wstringToQString(model->findStringProperty(GO_PLACEHOLDER_PROPERTY_NAME_STR)));
            }
            if (isNewWidget || model->hasChanged(GO_HORIZONTAL_ALIGNMENT_PROPERTY_NAME_STR)) {
                std::wstring horizontalAlignment
                    = model->findStringProperty(GO_HORIZONTAL_ALIGNMENT_PROPERTY_NAME_STR);
                Qt::Alignment alignment = Qt::AlignVCenter;
                if (horizontalAlignment == GO_PROPERTY_VALUE_RIGHT_STR) {
                    alignment |= Qt::AlignRight;
                } else if (horizontalAlignment == GO_PROPERTY_VALUE_CENTER_STR) {
                    alignment |= Qt::AlignHCenter;
                } else {
                    alignment |= Qt::AlignLeft;
                }
                lineEdit->setAlignment(alignment);
            }
        }
        //=============================================================================
    private:
        //=============================================================================
        void
        onTextEdited(const QString& text)
        {
            if (model->isProgrammaticChange()) {
                return;
            }
            wstringVector fieldNames = { L"Value" };
            ArrayOfVector fieldValues;
            fieldValues << ArrayOf::characterArray(QStringTowstring(text));
            model->emitCallbackEvent(GO_VALUE_CHANGING_FCN_PROPERTY_NAME_STR, L"ValueChangingData",
                L"ValueChanging", fieldNames, fieldValues);
        }
        //=============================================================================
        void
        onEditingFinished()
        {
            if (model->isProgrammaticChange()) {
                return;
            }
            QLineEdit* lineEdit = qobject_cast<QLineEdit*>(widget.data());
            if (!lineEdit) {
                return;
            }
            GOUIEditField* editField = static_cast<GOUIEditField*>(model);
            GOArrayOfProperty* valueProperty
                = static_cast<GOArrayOfProperty*>(model->findProperty(GO_VALUE_PROPERTY_NAME_STR));
            ArrayOf previousValue = valueProperty->data();
            std::wstring text = QStringTowstring(lineEdit->text());

            if (editField->isNumeric()) {
                double previousNumeric
                    = previousValue.isEmpty() ? 0.0 : ArrayOf(previousValue).asDoubleScalar();
                bool allowEmpty = model->stringCheck(
                    GO_ALLOW_EMPTY_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
                if (text.empty() && allowEmpty) {
                    valueProperty->data(ArrayOf::empty(0, 0));
                } else {
                    double parsed = 0;
                    try {
                        size_t consumed = 0;
                        parsed = std::stod(text, &consumed);
                        while (consumed < text.size() && iswspace(text[consumed])) {
                            ++consumed;
                        }
                        if (consumed != text.size()) {
                            throw std::invalid_argument("trailing");
                        }
                    } catch (...) {
                        lineEdit->setText(
                            wstringToQString(editField->formatNumericValue(previousNumeric)));
                        return;
                    }
                    if (model->stringCheck(GO_ROUND_FRACTIONAL_VALUES_PROPERTY_NAME_STR,
                            GO_PROPERTY_VALUE_ON_STR)) {
                        parsed = std::round(parsed);
                    }
                    std::vector<double> limits(
                        model->findVectorDoubleProperty(GO_LIMITS_PROPERTY_NAME_STR));
                    bool lowerInclusive = model->stringCheck(
                        GO_LOWER_LIMIT_INCLUSIVE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
                    bool upperInclusive = model->stringCheck(
                        GO_UPPER_LIMIT_INCLUSIVE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
                    bool inRange = limits.size() != 2
                        || ((lowerInclusive ? parsed >= limits[0] : parsed > limits[0])
                            && (upperInclusive ? parsed <= limits[1] : parsed < limits[1]));
                    if (!inRange || std::isnan(parsed)) {
                        lineEdit->setText(
                            wstringToQString(editField->formatNumericValue(previousNumeric)));
                        return;
                    }
                    valueProperty->data(ArrayOf::doubleScalar(parsed));
                    lineEdit->setText(wstringToQString(editField->formatNumericValue(parsed)));
                }
            } else {
                std::vector<double> characterLimits(
                    model->findVectorDoubleProperty(GO_CHARACTER_LIMITS_PROPERTY_NAME_STR));
                if (characterLimits.size() == 2) {
                    double length = (double)text.size();
                    if (length < characterLimits[0] || length > characterLimits[1]) {
                        std::wstring previousText
                            = previousValue.isEmpty() ? L"" : ArrayOf(previousValue).asWideString();
                        lineEdit->setText(wstringToQString(previousText));
                        return;
                    }
                }
                valueProperty->data(ArrayOf::characterArray(text));
            }

            ArrayOf newValue = valueProperty->data();
            wstringVector fieldNames = { L"Value", L"PreviousValue" };
            ArrayOfVector fieldValues;
            fieldValues << newValue;
            fieldValues << previousValue;
            model->emitCallbackEvent(GO_VALUE_CHANGED_FCN_PROPERTY_NAME_STR, L"ValueChangedData",
                L"ValueChanged", fieldNames, fieldValues);
        }
        //=============================================================================
    };
    //=============================================================================
} // namespace
//=============================================================================
UIComponentBinding*
createUIEditFieldBinding(GOUIComponent* component)
{
    return new UIEditFieldBinding(component);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
