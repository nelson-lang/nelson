//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <QtWidgets/QLabel>
#include "UIComponentBindings.hpp"
#include "UIComponentBindingBase.hpp"
#include "GOUILabel.h"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "QStringConverter.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
namespace {
    //=============================================================================
    class UILabelBinding : public QtUIComponentBinding
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
            QLabel* label = new QLabel(parent);
            widget = label;
        }
        //=============================================================================
        void
        applyStateToWidget(bool isNewWidget) override
        {
            QLabel* label = qobject_cast<QLabel*>(widget.data());
            if (!label) {
                return;
            }
            if (isNewWidget || model->hasChanged(GO_TEXT_PROPERTY_NAME_STR)
                || model->hasChanged(GO_INTERPRETER_PROPERTY_NAME_STR)) {
                wstringVector lines = model->findStringVectorProperty(GO_TEXT_PROPERTY_NAME_STR);
                std::wstring text;
                for (size_t k = 0; k < lines.size(); ++k) {
                    if (k > 0) {
                        text += L"\n";
                    }
                    text += lines[k];
                }
                if (model->stringCheck(GO_INTERPRETER_PROPERTY_NAME_STR, L"html")) {
                    label->setTextFormat(Qt::RichText);
                } else {
                    label->setTextFormat(Qt::PlainText);
                }
                label->setText(wstringToQString(text));
            }
            if (isNewWidget || model->hasChanged(GO_WORD_WRAP_PROPERTY_NAME_STR)) {
                label->setWordWrap(
                    model->stringCheck(GO_WORD_WRAP_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR));
            }
            if (isNewWidget || model->hasChanged(GO_HORIZONTAL_ALIGNMENT_PROPERTY_NAME_STR)
                || model->hasChanged(GO_VERTICAL_ALIGNMENT_PROPERTY_NAME_STR)) {
                Qt::Alignment alignment;
                std::wstring horizontalAlignment
                    = model->findStringProperty(GO_HORIZONTAL_ALIGNMENT_PROPERTY_NAME_STR);
                if (horizontalAlignment == GO_PROPERTY_VALUE_RIGHT_STR) {
                    alignment |= Qt::AlignRight;
                } else if (horizontalAlignment == GO_PROPERTY_VALUE_CENTER_STR) {
                    alignment |= Qt::AlignHCenter;
                } else {
                    alignment |= Qt::AlignLeft;
                }
                std::wstring verticalAlignment
                    = model->findStringProperty(GO_VERTICAL_ALIGNMENT_PROPERTY_NAME_STR);
                if (verticalAlignment == GO_PROPERTY_VALUE_TOP_STR) {
                    alignment |= Qt::AlignTop;
                } else if (verticalAlignment == GO_PROPERTY_VALUE_BOTTOM_STR) {
                    alignment |= Qt::AlignBottom;
                } else {
                    alignment |= Qt::AlignVCenter;
                }
                label->setAlignment(alignment);
            }
        }
        //=============================================================================
    };
    //=============================================================================
} // namespace
//=============================================================================
UIComponentBinding*
createUILabelBinding(GOUIComponent* component)
{
    return new UILabelBinding(component);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
