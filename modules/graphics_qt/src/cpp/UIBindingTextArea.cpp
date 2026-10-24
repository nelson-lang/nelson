//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <QtWidgets/QPlainTextEdit>
#include <QtGui/QFocusEvent>
#include "UIComponentBindings.hpp"
#include "UIComponentBindingBase.hpp"
#include "GOUITextArea.h"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "GOStringVectorProperty.hpp"
#include "QStringConverter.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
namespace {
    //=============================================================================
    class UITextAreaBinding;
    //=============================================================================
    class QUITextAreaWidget : public QPlainTextEdit
    {
        UITextAreaBinding* binding;

    public:
        QUITextAreaWidget(UITextAreaBinding* owner, QWidget* parent)
            : QPlainTextEdit(parent), binding(owner)
        {
        }

    protected:
        void
        focusOutEvent(QFocusEvent* event) override;
    };
    //=============================================================================
    class UITextAreaBinding : public QtUIComponentBinding
    {
    public:
        using QtUIComponentBinding::QtUIComponentBinding;
        //=============================================================================
        // called by the widget when it loses focus (commit gesture)
        void
        commitValue()
        {
            if (model->isProgrammaticChange()) {
                return;
            }
            QPlainTextEdit* textEdit = qobject_cast<QPlainTextEdit*>(widget.data());
            if (!textEdit) {
                return;
            }
            GOStringVectorProperty* valueProperty = static_cast<GOStringVectorProperty*>(
                model->findProperty(GO_VALUE_PROPERTY_NAME_STR));
            wstringVector previousLines = valueProperty->data();
            QString qtext = textEdit->toPlainText();
            QStringList qlines = qtext.split(QLatin1Char('\n'));
            wstringVector lines;
            lines.reserve((size_t)qlines.size());
            for (const QString& qline : qlines) {
                lines.push_back(QStringTowstring(qline));
            }
            if (lines == previousLines) {
                return;
            }
            valueProperty->data(lines);

            auto toCell = [](const wstringVector& entries) {
                ArrayOf* elements
                    = (ArrayOf*)ArrayOf::allocateArrayOf(NLS_CELL_ARRAY, entries.size());
                for (size_t k = 0; k < entries.size(); ++k) {
                    elements[k] = ArrayOf::characterArray(entries[k]);
                }
                return ArrayOf(NLS_CELL_ARRAY, Dimensions(entries.size(), 1), elements);
            };
            wstringVector fieldNames = { L"Value", L"PreviousValue" };
            ArrayOfVector fieldValues;
            fieldValues << toCell(lines);
            fieldValues << toCell(previousLines);
            model->emitCallbackEvent(GO_VALUE_CHANGED_FCN_PROPERTY_NAME_STR, L"ValueChangedData",
                L"ValueChanged", fieldNames, fieldValues);
        }
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
            QUITextAreaWidget* textEdit = new QUITextAreaWidget(this, parent);
            QObject::connect(textEdit, &QPlainTextEdit::textChanged, textEdit,
                [this]() { notifyTextChanging(); });
            widget = textEdit;
        }
        //=============================================================================
        void
        applyStateToWidget(bool isNewWidget) override
        {
            QPlainTextEdit* textEdit = qobject_cast<QPlainTextEdit*>(widget.data());
            if (!textEdit) {
                return;
            }
            if (isNewWidget || model->hasChanged(GO_VALUE_PROPERTY_NAME_STR)) {
                wstringVector lines = model->findStringVectorProperty(GO_VALUE_PROPERTY_NAME_STR);
                std::wstring text;
                for (size_t k = 0; k < lines.size(); ++k) {
                    if (k > 0) {
                        text += L"\n";
                    }
                    text += lines[k];
                }
                textEdit->setPlainText(wstringToQString(text));
            }
            if (isNewWidget || model->hasChanged(GO_EDITABLE_PROPERTY_NAME_STR)) {
                textEdit->setReadOnly(
                    !model->stringCheck(GO_EDITABLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR));
            }
            if (isNewWidget || model->hasChanged(GO_WORD_WRAP_PROPERTY_NAME_STR)) {
                textEdit->setLineWrapMode(
                    model->stringCheck(GO_WORD_WRAP_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR)
                        ? QPlainTextEdit::WidgetWidth
                        : QPlainTextEdit::NoWrap);
            }
            if (isNewWidget || model->hasChanged(GO_PLACEHOLDER_PROPERTY_NAME_STR)) {
                textEdit->setPlaceholderText(
                    wstringToQString(model->findStringProperty(GO_PLACEHOLDER_PROPERTY_NAME_STR)));
            }
        }
        //=============================================================================
    private:
        //=============================================================================
        void
        notifyTextChanging()
        {
            if (model->isProgrammaticChange()) {
                return;
            }
            QPlainTextEdit* textEdit = qobject_cast<QPlainTextEdit*>(widget.data());
            if (!textEdit) {
                return;
            }
            std::wstring text = QStringTowstring(textEdit->toPlainText());
            wstringVector fieldNames = { L"Value" };
            ArrayOfVector fieldValues;
            fieldValues << ArrayOf::characterArray(text);
            model->emitCallbackEvent(GO_VALUE_CHANGING_FCN_PROPERTY_NAME_STR, L"ValueChangingData",
                L"ValueChanging", fieldNames, fieldValues);
        }
        //=============================================================================
    };
    //=============================================================================
    void
    QUITextAreaWidget::focusOutEvent(QFocusEvent* event)
    {
        QPlainTextEdit::focusOutEvent(event);
        if (binding) {
            binding->commitValue();
        }
    }
    //=============================================================================
} // namespace
//=============================================================================
UIComponentBinding*
createUITextAreaBinding(GOUIComponent* component)
{
    return new UITextAreaBinding(component);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
