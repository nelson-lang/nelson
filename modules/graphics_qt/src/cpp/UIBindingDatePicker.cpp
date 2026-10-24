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
#include <QtWidgets/QDateEdit>
#include "UIComponentBindings.hpp"
#include "UIComponentBindingBase.hpp"
#include "GOUIDatePicker.h"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "QStringConverter.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
namespace {
    //=============================================================================
    // Serial datenum <-> Julian day offset (datenum(2026,7,18) = 740181,
    // QDate(2026,7,18).toJulianDay() = 2461240)
    const qint64 JULIAN_DAY_OFFSET = 1721059;
    //=============================================================================
    double
    serialOfDatetime(const ArrayOf& value, bool& valid)
    {
        valid = false;
        if (!value.isClassType()) {
            return std::nan("");
        }
        ArrayOf copied(value);
        ArrayOf serial = copied.field("SerialValue");
        if (serial.isEmpty() || !serial.isNumeric()) {
            return std::nan("");
        }
        double serialValue = ArrayOf(serial).asDoubleScalar();
        valid = std::isfinite(serialValue);
        return serialValue;
    }
    //=============================================================================
    QString
    ldmlToQtFormat(const std::wstring& ldml)
    {
        // LDML uses 'uuuu' for the year; Qt uses 'yyyy'
        QString format = wstringToQString(ldml);
        format.replace(QStringLiteral("uuuu"), QStringLiteral("yyyy"));
        format.replace(QStringLiteral("u"), QStringLiteral("y"));
        return format;
    }
    //=============================================================================
    class UIDatePickerBinding : public QtUIComponentBinding
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
            QDateEdit* dateEdit = new QDateEdit(parent);
            dateEdit->setCalendarPopup(true);
            // Expected month abbreviations are English regardless of the system locale
            dateEdit->setLocale(QLocale(QLocale::English, QLocale::UnitedStates));
            QObject::connect(
                dateEdit, &QDateEdit::dateChanged, dateEdit, [this](const QDate& date) {
                    static_cast<GOUIDatePicker*>(model)->onUserDateChanged(
                        (double)(date.toJulianDay() - JULIAN_DAY_OFFSET));
                });
            widget = dateEdit;
        }
        //=============================================================================
        void
        applyStateToWidget(bool isNewWidget) override
        {
            QDateEdit* dateEdit = qobject_cast<QDateEdit*>(widget.data());
            if (!dateEdit) {
                return;
            }
            if (isNewWidget || model->hasChanged(GO_DISPLAY_FORMAT_PROPERTY_NAME_STR)) {
                dateEdit->setDisplayFormat(
                    ldmlToQtFormat(model->findStringProperty(GO_DISPLAY_FORMAT_PROPERTY_NAME_STR)));
            }
            if (isNewWidget || model->hasChanged(GO_VALUE_PROPERTY_NAME_STR)) {
                bool valid = false;
                double serialValue = serialOfDatetime(
                    model->findArrayOfProperty(GO_VALUE_PROPERTY_NAME_STR), valid);
                if (valid) {
                    dateEdit->setDate(
                        QDate::fromJulianDay((qint64)serialValue + JULIAN_DAY_OFFSET));
                }
            }
            if (isNewWidget || model->hasChanged(GO_EDITABLE_PROPERTY_NAME_STR)) {
                dateEdit->setReadOnly(
                    !model->stringCheck(GO_EDITABLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR));
            }
        }
        //=============================================================================
    };
    //=============================================================================
} // namespace
//=============================================================================
UIComponentBinding*
createUIDatePickerBinding(GOUIComponent* component)
{
    return new UIDatePickerBinding(component);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
