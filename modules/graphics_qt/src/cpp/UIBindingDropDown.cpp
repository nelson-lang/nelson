//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <QtWidgets/QComboBox>
#include <QtWidgets/QLineEdit>
#include "UIComponentBindings.hpp"
#include "UIComponentBindingBase.hpp"
#include "GOUIDropDown.h"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "QStringConverter.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
namespace {
    //=============================================================================
    class UIDropDownBinding : public QtUIComponentBinding
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
            QComboBox* comboBox = new QComboBox(parent);
            QObject::connect(comboBox, QOverload<int>::of(&QComboBox::activated), comboBox,
                [this](int index) { static_cast<GOUIDropDown*>(model)->onActivated(index); });
            widget = comboBox;
        }
        //=============================================================================
        void
        applyStateToWidget(bool isNewWidget) override
        {
            QComboBox* comboBox = qobject_cast<QComboBox*>(widget.data());
            if (!comboBox) {
                return;
            }
            bool itemsChanged = model->hasChanged(GO_ITEMS_PROPERTY_NAME_STR)
                || model->hasChanged(GO_ITEMS_DATA_PROPERTY_NAME_STR);
            bool valueChanged = model->hasChanged(GO_VALUE_PROPERTY_NAME_STR)
                || model->hasChanged(GO_VALUE_INDEX_PROPERTY_NAME_STR);
            if (isNewWidget || itemsChanged || valueChanged) {
                static_cast<GOUIDropDown*>(model)->synchronizeValueWithItems();
            }
            if (isNewWidget || itemsChanged) {
                wstringVector items = model->findStringVectorProperty(GO_ITEMS_PROPERTY_NAME_STR);
                comboBox->clear();
                for (const std::wstring& item : items) {
                    comboBox->addItem(wstringToQString(item));
                }
            }
            if (isNewWidget || model->hasChanged(GO_EDITABLE_PROPERTY_NAME_STR)) {
                comboBox->setEditable(
                    model->stringCheck(GO_EDITABLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR));
            }
            if (comboBox->isEditable()
                && (isNewWidget || model->hasChanged(GO_PLACEHOLDER_PROPERTY_NAME_STR))) {
                comboBox->lineEdit()->setPlaceholderText(
                    wstringToQString(model->findStringProperty(GO_PLACEHOLDER_PROPERTY_NAME_STR)));
            }
            if (isNewWidget || itemsChanged || valueChanged) {
                int index = (int)model->findScalarDoubleProperty(GO_VALUE_INDEX_PROPERTY_NAME_STR);
                if (index >= 1 && index <= comboBox->count()) {
                    comboBox->setCurrentIndex(index - 1);
                }
            }
        }
        //=============================================================================
    };
    //=============================================================================
} // namespace
//=============================================================================
UIComponentBinding*
createUIDropDownBinding(GOUIComponent* component)
{
    return new UIDropDownBinding(component);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
