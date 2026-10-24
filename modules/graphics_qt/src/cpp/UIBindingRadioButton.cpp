//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <QtWidgets/QAbstractButton>
#include <QtWidgets/QPushButton>
#include <QtWidgets/QRadioButton>
#include "UIComponentBindings.hpp"
#include "UIComponentBindingBase.hpp"
#include "GOUIRadioButton.h"
#include "GOUIPanel.h"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "GOGObjectsProperty.hpp"
#include "GOScalarLogicalProperty.hpp"
#include "GOHelpers.hpp"
#include "QStringConverter.hpp"
#include "UIAlignedPushButton.h"
//=============================================================================
namespace Nelson {
//=============================================================================
namespace {
    //=============================================================================
    class UIRadioButtonBinding : public QtUIComponentBinding
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
            QAbstractButton* button = nullptr;
            if (static_cast<GOUIRadioButton*>(model)->isToggleButton()) {
                QPushButton* pushButton = new UIAlignedPushButton(parent);
                pushButton->setCheckable(true);
                button = pushButton;
            } else {
                QRadioButton* radioButton = new QRadioButton(parent);
                // exclusivity is managed by the Nelson button group, not by Qt
                radioButton->setAutoExclusive(false);
                button = radioButton;
            }
            QObject::connect(button, &QAbstractButton::clicked, button, [this]() { onClicked(); });
            widget = button;

            // the first button added to a group becomes the selected one
            GOUIPanel* group = static_cast<GOUIRadioButton*>(model)->parentButtonGroup();
            if (group) {
                GOGObjectsProperty* selectedObject = static_cast<GOGObjectsProperty*>(
                    group->findProperty(GO_SELECTED_OBJECT_PROPERTY_NAME_STR, false));
                if (selectedObject && selectedObject->data().empty()) {
                    GOScalarLogicalProperty* value = static_cast<GOScalarLogicalProperty*>(
                        model->findProperty(GO_VALUE_PROPERTY_NAME_STR));
                    value->data(true);
                    int64 thisHandle = findGraphicsObjectHandle(model);
                    std::vector<int64> selection;
                    selection.push_back(thisHandle);
                    selectedObject->data(selection);
                }
            }
        }
        //=============================================================================
        void
        applyStateToWidget(bool isNewWidget) override
        {
            QAbstractButton* button = qobject_cast<QAbstractButton*>(widget.data());
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
            UIAlignedPushButton* toggleButton = dynamic_cast<UIAlignedPushButton*>(button);
            if (toggleButton
                && (isNewWidget || model->hasChanged(GO_VERTICAL_ALIGNMENT_PROPERTY_NAME_STR))) {
                toggleButton->setVerticalAlignment(
                    model->findStringProperty(GO_VERTICAL_ALIGNMENT_PROPERTY_NAME_STR));
            }
            if (isNewWidget || model->hasChanged(GO_VALUE_PROPERTY_NAME_STR)) {
                GOScalarLogicalProperty* value = static_cast<GOScalarLogicalProperty*>(
                    model->findProperty(GO_VALUE_PROPERTY_NAME_STR));
                button->setChecked(value->data());
                if (model->hasChanged(GO_VALUE_PROPERTY_NAME_STR) && value->data()) {
                    // programmatic selection: update the group without firing events
                    GOUIPanel* group = static_cast<GOUIRadioButton*>(model)->parentButtonGroup();
                    if (group) {
                        int64 thisHandle = findGraphicsObjectHandle(model);
                        group->notifyChildSelected(thisHandle, false);
                    }
                }
            }
        }
        //=============================================================================
        QString
        additionalStyleSheet() override
        {
            if (static_cast<GOUIRadioButton*>(model)->isToggleButton()) {
                // toggle buttons render like push buttons with a darker pressed face
                return QStringLiteral(
                    "QPushButton { background-color: #f5f5f5; border: 1px solid #8f8f8f;"
                    " border-radius: 4px; }"
                    "QPushButton:checked { background-color: #d9d9d9; }");
            }
            // blend with the parent (no BackgroundColor property): a style sheet otherwise
            // makes Qt paint the default light background, hiding a light FontColor.
            // native app look: white circle with gray border, black dot when selected
            return QStringLiteral(
                "QRadioButton { background: transparent; }"
                "QRadioButton::indicator { width: 13px; height: 13px; border: 1px solid #8f8f8f;"
                " border-radius: 7px; background: white; }"
                "QRadioButton::indicator:checked { background: qradialgradient(cx:0.5, cy:0.5,"
                " radius:0.5, fx:0.5, fy:0.5, stop:0 #1a1a1a, stop:0.42 #1a1a1a, stop:0.52 #ffffff,"
                " stop:1 #ffffff); }");
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
            QAbstractButton* button = qobject_cast<QAbstractButton*>(widget.data());
            if (!button) {
                return;
            }
            static_cast<GOUIRadioButton*>(model)->onUserClick();
            button->setChecked(true);
        }
        //=============================================================================
    };
    //=============================================================================
} // namespace
//=============================================================================
UIComponentBinding*
createUIRadioButtonBinding(GOUIComponent* component)
{
    return new UIRadioButtonBinding(component);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
