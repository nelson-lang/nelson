//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <QtWidgets/QCheckBox>
#include "UIComponentBindings.hpp"
#include "UIComponentBindingBase.hpp"
#include "GOUICheckBox.h"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "GOScalarLogicalProperty.hpp"
#include "QStringConverter.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
namespace {
    //=============================================================================
    class UICheckBoxBinding : public QtUIComponentBinding
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
            QCheckBox* checkBox = new QCheckBox(parent);
            QObject::connect(checkBox, &QCheckBox::clicked, checkBox, [this]() { onClicked(); });
            widget = checkBox;
        }
        //=============================================================================
        void
        applyStateToWidget(bool isNewWidget) override
        {
            QCheckBox* checkBox = qobject_cast<QCheckBox*>(widget.data());
            if (!checkBox) {
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
                checkBox->setText(wstringToQString(text));
            }
            if (isNewWidget || model->hasChanged(GO_VALUE_PROPERTY_NAME_STR)) {
                GOScalarLogicalProperty* value = static_cast<GOScalarLogicalProperty*>(
                    model->findProperty(GO_VALUE_PROPERTY_NAME_STR));
                checkBox->setChecked(value->data());
            }
        }
        //=============================================================================
        QString
        additionalStyleSheet() override
        {
            // The check box has no BackgroundColor property, so it must blend with its
            // parent: without this rule Qt paints the default (light) widget background
            // once a style sheet is set, hiding a light FontColor on a dark figure.
            // App-style look: white indicator box with a thin gray border; checked state
            // shows a check mark (falls back to a filled blue box when the style image is
            // unavailable).
            return QStringLiteral(
                "QCheckBox { background: transparent; }"
                "QCheckBox::indicator { width: 13px; height: 13px; border: 1px solid #8f8f8f;"
                " border-radius: 2px; background: white; }"
                "QCheckBox::indicator:checked {"
                " image: "
                "url(:/qt-project.org/styles/commonstyle/images/standardbutton-apply-16.png); }");
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
            QCheckBox* checkBox = qobject_cast<QCheckBox*>(widget.data());
            if (!checkBox) {
                return;
            }
            GOScalarLogicalProperty* value = static_cast<GOScalarLogicalProperty*>(
                model->findProperty(GO_VALUE_PROPERTY_NAME_STR));
            bool previousValue = value->data();
            bool newValue = checkBox->isChecked();
            value->data(newValue);
            wstringVector fieldNames = { L"Value", L"PreviousValue" };
            ArrayOfVector fieldValues;
            fieldValues << ArrayOf::logicalScalar(newValue);
            fieldValues << ArrayOf::logicalScalar(previousValue);
            model->emitCallbackEvent(GO_VALUE_CHANGED_FCN_PROPERTY_NAME_STR, L"ValueChangedData",
                L"ValueChanged", fieldNames, fieldValues);
        }
        //=============================================================================
    };
    //=============================================================================
} // namespace
//=============================================================================
UIComponentBinding*
createUICheckBoxBinding(GOUIComponent* component)
{
    return new UICheckBoxBinding(component);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
