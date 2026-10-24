//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <QtCore/QPointer>
#include <QtGui/QImage>
#include <QtGui/QKeyEvent>
#include <QtGui/QKeySequence>
#include <QtGui/QPainter>
#include <QtGui/QPixmap>
#include <QtWidgets/QPushButton>
#include <QtWidgets/QRadioButton>
#include <QtWidgets/QCheckBox>
#include <QtWidgets/QSlider>
#include <QtWidgets/QLabel>
#include <QtWidgets/QLineEdit>
#include <QtWidgets/QComboBox>
#include <QtWidgets/QListWidget>
#include "UIControlBinding.hpp"
#include "UIComponentBindingBase.hpp"
#include "GOUIComponentUnits.hpp"
#include "GOUIControl.h"
#include "GOPropertyValues.hpp"
#include "GOPropertyNames.hpp"
#include "GOColorProperty.hpp"
#include "GOStringProperty.hpp"
#include "GOStringVectorProperty.hpp"
#include "GOScalarDoubleProperty.hpp"
#include "GOFontAngleProperty.hpp"
#include "GOFontUnitsProperty.hpp"
#include "GOFontWeightProperty.hpp"
#include "GOAlignHorizProperty.hpp"
#include "GOUnitsProperty.hpp"
#include "GOFigure.hpp"
#include "GOVectorProperty.hpp"
#include "GOCallbackProperty.hpp"
#include "GORoot.hpp"
#include "GOHelpers.hpp"
#include "RenderQtHelpers.hpp"
#include "UnitsConversionHelpers.hpp"
#include "QStringConverter.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "QPushButtonWidget.h"
#include "QToggleButtonWidget.h"
#include "QCheckBoxWidget.h"
#include "QRadioButtonWidget.h"
#include "QSliderWidget.h"
#include "QLabelWidget.h"
#include "QPopupMenuWidget.h"
#include "QLineEditWidget.h"
#include "QMultiLineEdit.h"
#include "QMultiLineEditWidget.h"
#include "QListBoxWidget.h"
//=============================================================================
namespace Nelson {
//=============================================================================
static QString
colorStyle(const QString& name, GOColorProperty* color)
{
    if (!color || color->isNone()) {
        return {};
    }
    std::vector<double> rgb = color->data();
    QColor qColor = QColor::fromRgb(rgb[0] * 255, rgb[1] * 255, rgb[2] * 255);
    return QString("%1: %2;").arg(name, qColor.name());
}
//=============================================================================
static QString
fontSizeStyle(GOUIControl* control)
{
    GOScalarProperty* size
        = static_cast<GOScalarProperty*>(control->findProperty(GO_FONT_SIZE_PROPERTY_NAME_STR));
    GOFontUnitsProperty* units
        = static_cast<GOFontUnitsProperty*>(control->findProperty(GO_FONT_UNITS_PROPERTY_NAME_STR));
    if (!size || !units) {
        return {};
    }
    double fontSize = size->data();
    std::wstring unit = units->data();
    if (unit == GO_PROPERTY_VALUE_PIXELS_STR) {
        return QString("font-size: %1px;").arg(fontSize);
    }
    if (unit == GO_PROPERTY_VALUE_POINTS_STR) {
        return QString("font-size: %1pt;").arg(fontSize);
    }
    if (unit == GO_PROPERTY_VALUE_CENTIMETERS_STR) {
        return QString("font-size: %1pt;").arg(fontSize * 28.3465);
    }
    if (unit == GO_PROPERTY_VALUE_INCHES_STR) {
        return QString("font-size: %1pt;").arg(fontSize * 72.0);
    }
    return {};
}
//=============================================================================
static QString
buildWidgetStyleSheet(GOUIControl* control)
{
    QString styleSheet;
    if (control->findArrayOfProperty(GO_C_DATA_PROPERTY_NAME_STR).isEmpty()) {
        styleSheet += colorStyle(QStringLiteral("background-color"),
            static_cast<GOColorProperty*>(
                control->findProperty(GO_BACKGROUND_COLOR_PROPERTY_NAME_STR)));
    }
    styleSheet += colorStyle(QStringLiteral("color"),
        static_cast<GOColorProperty*>(
            control->findProperty(GO_FOREGROUND_COLOR_PROPERTY_NAME_STR)));

    GOStringProperty* fontName
        = static_cast<GOStringProperty*>(control->findProperty(GO_FONT_NAME_PROPERTY_NAME_STR));
    if (fontName) {
        styleSheet += QString("font-family: %1;")
                          .arg(wstringToQString(resolveRootFontName(fontName->data())));
    }
    styleSheet += fontSizeStyle(control);

    GOFontAngleProperty* fontAngle
        = static_cast<GOFontAngleProperty*>(control->findProperty(GO_FONT_ANGLE_PROPERTY_NAME_STR));
    if (fontAngle) {
        if (fontAngle->isEqual(GO_PROPERTY_VALUE_ITALIC_STR)) {
            styleSheet += QStringLiteral("font-style: italic;");
        } else if (fontAngle->isEqual(GO_PROPERTY_VALUE_OBLIQUE_STR)) {
            styleSheet += QStringLiteral("font-style: oblique;");
        } else {
            styleSheet += QStringLiteral("font-style: normal;");
        }
    }

    GOFontWeightProperty* fontWeight = static_cast<GOFontWeightProperty*>(
        control->findProperty(GO_FONT_WEIGHT_PROPERTY_NAME_STR));
    if (fontWeight) {
        if (fontWeight->isEqual(GO_PROPERTY_VALUE_BOLD_STR)) {
            styleSheet += QStringLiteral("font-weight: bold;");
        } else if (fontWeight->isEqual(GO_PROPERTY_VALUE_LIGHT_STR)) {
            styleSheet += QStringLiteral("font-weight: 300;");
        } else if (fontWeight->isEqual(GO_PROPERTY_VALUE_DEMI_STR)) {
            styleSheet += QStringLiteral("font-weight: 600;");
        } else {
            styleSheet += QStringLiteral("font-weight: normal;");
        }
    }

    if (control->stringCheck(GO_STYLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_FRAME_STR)) {
        // Any stylesheet suppresses QFrame's native border, so paint the frame outline
        // explicitly (ForegroundColor).
        GOColorProperty* foreground = static_cast<GOColorProperty*>(
            control->findProperty(GO_FOREGROUND_COLOR_PROPERTY_NAME_STR));
        QString border = QStringLiteral("gray");
        if (foreground && !foreground->isNone()) {
            std::vector<double> rgb = foreground->data();
            border = QColor::fromRgb(rgb[0] * 255, rgb[1] * 255, rgb[2] * 255).name();
        }
        styleSheet += QString("border: 1px solid %1;").arg(border);
    }

    if (control->stringCheck(GO_STYLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_TEXT_STR)
        || control->stringCheck(GO_STYLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_EDIT_STR)) {
        GOAlignHorizProperty* textAlign = static_cast<GOAlignHorizProperty*>(
            control->findProperty(GO_HORIZONTAL_ALIGNMENT_PROPERTY_NAME_STR));
        if (textAlign) {
            if (textAlign->isEqual(GO_PROPERTY_VALUE_LEFT_STR)) {
                styleSheet += QStringLiteral("text-align: left;");
            } else if (textAlign->isEqual(GO_PROPERTY_VALUE_RIGHT_STR)) {
                styleSheet += QStringLiteral("text-align: right;");
            } else {
                styleSheet += QStringLiteral("text-align: center;");
            }
        }
    }

    const bool isButtonFamily
        = control->stringCheck(GO_STYLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_PUSHBUTTON_STR)
        || control->stringCheck(GO_STYLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_TOGGLEBUTTON_STR);
    if (isButtonFamily) {
        GOColorProperty* background = static_cast<GOColorProperty*>(
            control->findProperty(GO_BACKGROUND_COLOR_PROPERTY_NAME_STR));
        const bool hasBackground
            = control->findArrayOfProperty(GO_C_DATA_PROPERTY_NAME_STR).isEmpty() && background
            && !background->isNone();
        if (hasBackground) {
            // A background-color stylesheet drops Qt's native pressed/hover
            // feedback, so restore it: a press darkens the fill, a hover nudges
            // it, and a latched togglebutton keeps the pressed shade.
            std::vector<double> rgb = background->data();
            QColor base(static_cast<int>(rgb[0] * 255.0), static_cast<int>(rgb[1] * 255.0),
                static_cast<int>(rgb[2] * 255.0));
            const QString hover = base.darker(106).name();
            const QString pressed = base.darker(122).name();
            QString rules = QStringLiteral("QPushButton { %1 }").arg(styleSheet);
            rules += QStringLiteral(" QPushButton:hover { background-color: %1; }").arg(hover);
            rules += QStringLiteral(" QPushButton:pressed { background-color: %1; }").arg(pressed);
            if (control->stringCheck(
                    GO_STYLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_TOGGLEBUTTON_STR)) {
                rules += QStringLiteral(" QPushButton:checked { background-color: %1; }")
                             .arg(pressed);
            }
            return rules;
        }
    }

    // Any stylesheet makes Qt draw the check box / radio indicator through the style-sheet
    // engine. Without an explicit ::indicator rule the native box is dropped and only a bare
    // check mark shows. Draw it explicitly: a white box/circle with a thin gray border, plus
    // a check mark or filled dot when selected.
    if (control->stringCheck(GO_STYLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_CHECKBOX_STR)) {
        QString rules = QStringLiteral("QCheckBox { %1 }").arg(styleSheet);
        rules += QStringLiteral(
            " QCheckBox::indicator { width: 13px; height: 13px; border: 1px solid #8f8f8f;"
            " border-radius: 2px; background: white; }"
            " QCheckBox::indicator:checked { image:"
            " url(:/qt-project.org/styles/commonstyle/images/standardbutton-apply-16.png); }");
        return rules;
    }
    if (control->stringCheck(GO_STYLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_RADIOBUTTON_STR)) {
        QString rules = QStringLiteral("QRadioButton { %1 }").arg(styleSheet);
        rules += QStringLiteral(
            " QRadioButton::indicator { width: 13px; height: 13px; border: 1px solid #8f8f8f;"
            " border-radius: 7px; background: white; }"
            " QRadioButton::indicator:checked { background: qradialgradient(cx:0.5, cy:0.5,"
            " radius:0.5, fx:0.5, fy:0.5, stop:0 #1a1a1a, stop:0.42 #1a1a1a, stop:0.52 #ffffff,"
            " stop:1 #ffffff); }");
        return rules;
    }
    return styleSheet;
}
//=============================================================================
namespace {
    //=============================================================================
    // Qt widget backend of the classic uicontrol. One binding owns the QWidget
    // matching the model's Style.
    class UIControlBinding : public UIComponentBinding
    {
        GOUIControl* model;
        QPointer<QWidget> widget;

    public:
        //=============================================================================
        explicit UIControlBinding(GOUIControl* control) : model(control) { }
        //=============================================================================
        ~UIControlBinding() override { destroy(); }
        //=============================================================================
        bool
        create() override
        {
            QWidget* parent = getParentQWidget();
            if (!parent) {
                return false;
            }
            if (model->stringCheck(GO_STYLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_PUSHBUTTON_STR)) {
                widget = new QPushButtonWidget(model, parent);
            } else if (model->stringCheck(
                           GO_STYLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_TOGGLEBUTTON_STR)) {
                widget = new QToggleButtonWidget(model, parent);
            } else if (model->stringCheck(
                           GO_STYLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_RADIOBUTTON_STR)) {
                widget = new QRadioButtonWidget(model, parent);
            } else if (model->stringCheck(
                           GO_STYLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_CHECKBOX_STR)) {
                widget = new QCheckBoxWidget(model, parent);
            } else if (model->stringCheck(
                           GO_STYLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_SLIDER_STR)) {
                std::vector<double> sizevec(
                    model->findVectorDoubleProperty(GO_POSITION_PROPERTY_NAME_STR));
                widget = new QSliderWidget(sizevec[3] > sizevec[2], model, parent);
            } else if (model->stringCheck(GO_STYLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_TEXT_STR)) {
                widget = new QLabelWidget(model, parent);
            } else if (model->stringCheck(
                           GO_STYLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_FRAME_STR)) {
                // A frame is a decorative filled, bordered rectangle. QLabel is a
                // QFrame, so it reuses the label backend (String is not painted for
                // this Style); the border is drawn from the stylesheet because Qt
                // ignores the native frame shape once a background-color is set.
                widget = new QLabelWidget(model, parent);
            } else if (model->stringCheck(GO_STYLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_EDIT_STR)) {
                double min(model->findScalarDoubleProperty(GO_MIN_PROPERTY_NAME_STR));
                double max(model->findScalarDoubleProperty(GO_MAX_PROPERTY_NAME_STR));
                bool isMultiLine = (max - min) > 1;
                if (isMultiLine) {
                    widget = new QMultiLineEditWidget(model, parent);
                } else {
                    widget = new QLineEditWidget(model, parent);
                }
            } else if (model->stringCheck(
                           GO_STYLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_LISTBOX_STR)) {
                widget = new QListBoxWidget(model, parent);
            } else if (model->stringCheck(
                           GO_STYLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_POPUPMENU_STR)) {
                widget = new QPopupMenuWidget(model, parent);
            }
            if (widget) {
                widget->show();
                widget->raise();
                return true;
            }
            return false;
        }
        //=============================================================================
        bool
        exists() const override
        {
            return !widget.isNull();
        }
        //=============================================================================
        void
        applyCommonProperties(bool isNewWidget) override
        {
            // applyState applies everything for the classic uicontrol.
        }
        //=============================================================================
        void
        applyState(bool isNewWidget) override
        {
            onStringPropertyChanged(isNewWidget);
            onParentPositionChanged(isNewWidget);
            onVisiblePropertyChanged(isNewWidget);
            onBackgroundPropertyChanged(isNewWidget);
            onForegroundPropertyChanged(isNewWidget);

            onFontNameChanged(isNewWidget);
            onFontSizeOrUnitChanged(isNewWidget);
            onFontAngleChanged(isNewWidget);
            onFontWeightChanged(isNewWidget);
            onHAlignmentChanged(isNewWidget);

            onEnableChanged(isNewWidget);
            onToolTipsChanged(isNewWidget);
            onRadioButtonChanged(isNewWidget);
            onCheckBoxChanged(isNewWidget);
            onSliderChanged(isNewWidget);
            onToggleButtonChanged(isNewWidget);
            onEditPropertyChanged(isNewWidget);
            onCDataPropertyChanged(isNewWidget);
            onListTopBoxChanged(isNewWidget);
            onPopupMenuChanged(isNewWidget);

            onValueChanged(isNewWidget);
            model->onUnitsChanged(isNewWidget);
            onPositionPropertyChanged(isNewWidget);
        }
        //=============================================================================
        void
        show() override
        {
            if (widget) {
                widget->show();
            }
        }
        //=============================================================================
        void
        hide() override
        {
            if (widget) {
                widget->hide();
            }
        }
        //=============================================================================
        void
        setFocus() override
        {
            if (widget) {
                widget->raise();
                widget->setFocus();
            }
        }
        //=============================================================================
        void
        destroy() override
        {
            if (widget) {
                widget->hide();
                delete widget.data();
                widget.clear();
            }
        }
        //=============================================================================
        void*
        nativeWidget() override
        {
            return widget.data();
        }
        //=============================================================================
        bool
        consumeGeometryChange() override
        {
            return false;
        }
        //=============================================================================
        void
        renderSnapshot(RenderInterface& gc) override
        {
            if (!widget) {
                return;
            }
            std::vector<double> sizevec;
            if (!positionInPixels(sizevec, gc.width(), gc.height())) {
                return;
            }
            applyWidgetGeometry(sizevec, gc.height());
            QImage image(widget->size(), QImage::Format_ARGB32);
            QPainter painter(&image);
            widget->render(&painter);
            int y = gc.height() - sizevec[1] - widget->height();
            QImageRgba rgba(image);
            gc.drawImage(sizevec[0], y, rgba.view());
        }
        //=============================================================================
    private:
        //=============================================================================
        QWidget*
        getParentQWidget()
        {
            return uiParentQWidget(model, model->getParentFigureHost());
        }
        //=============================================================================
        void
        parentLayoutSize(double& width, double& height)
        {
            width = 0;
            height = 0;
            // held by a uipanel/uitab: the frame is the container widget
            std::vector<double> content;
            if (uiParentContentRect(model, content, height)) {
                width = content[2];
                return;
            }
            QWidget* parent = getParentQWidget();
            if (parent) {
                width = parent->width();
                height = parent->height();
            }
            GOFigureHost* host = model->getParentFigureHost();
            GOFigure* figure = host ? host->hostFigure() : nullptr;
            if (figure && figure->getLayoutWidth() > 0 && figure->getLayoutHeight() > 0) {
                width = figure->getLayoutWidth();
                height = figure->getLayoutHeight();
            }
        }
        //=============================================================================
        bool
        positionInPixels(std::vector<double>& sizevec, double width, double height)
        {
            sizevec = model->findVectorDoubleProperty(GO_POSITION_PROPERTY_NAME_STR);
            if (sizevec.size() < 4) {
                return false;
            }
            uiComponentPositionInParentFrame(model, sizevec, width, height);
            return true;
        }
        //=============================================================================
        void
        applyWidgetGeometry(const std::vector<double>& sizevec, double height)
        {
            int x = (int)sizevec[0];
            int y = (int)height - (int)sizevec[1] - (int)sizevec[3];
            widget->setGeometry(x, y, (int)sizevec[2], (int)sizevec[3]);
            widget->setFixedSize((int)sizevec[2], (int)sizevec[3]);
        }
        //=============================================================================
        void
        onStringChangedPushButton(bool newWidget)
        {
            if (!model->stringCheck(GO_STYLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_PUSHBUTTON_STR)) {
                return;
            }
            if (!model->hasChanged(GO_STRING_PROPERTY_NAME_STR) && !newWidget) {
                return;
            }
            wstringVector vstr = model->findStringVectorProperty(GO_STRING_PROPERTY_NAME_STR);
            std::wstring str = vstr.size() > 0 ? vstr[0] : L"";
            ((QPushButtonWidget*)widget.data())->setText(str);
            model->clearChanged(GO_STRING_PROPERTY_NAME_STR);
        }
        //=============================================================================
        void
        onStringChangedToggleButton(bool newWidget)
        {
            if (!model->stringCheck(
                    GO_STYLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_TOGGLEBUTTON_STR)) {
                return;
            }
            if (!model->hasChanged(GO_STRING_PROPERTY_NAME_STR) && !newWidget) {
                return;
            }
            wstringVector vstr = model->findStringVectorProperty(GO_STRING_PROPERTY_NAME_STR);
            std::wstring str = vstr.size() > 0 ? vstr[0] : L"";
            ((QPushButton*)widget.data())->setText(wstringToQString(str));
            model->clearChanged(GO_STRING_PROPERTY_NAME_STR);
        }
        //=============================================================================
        void
        onStringChangedRadioButton(bool newWidget)
        {
            if (!model->stringCheck(
                    GO_STYLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_RADIOBUTTON_STR)) {
                return;
            }
            if (!model->hasChanged(GO_STRING_PROPERTY_NAME_STR) && !newWidget) {
                return;
            }
            wstringVector vstr = model->findStringVectorProperty(GO_STRING_PROPERTY_NAME_STR);
            std::wstring str = vstr.size() > 0 ? vstr[0] : L"";
            ((QRadioButton*)widget.data())->setText(wstringToQString(str));
            model->clearChanged(GO_STRING_PROPERTY_NAME_STR);
        }
        //=============================================================================
        void
        onStringChangedCheckBox(bool newWidget)
        {
            if (!model->stringCheck(GO_STYLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_CHECKBOX_STR)) {
                return;
            }
            if (!model->hasChanged(GO_STRING_PROPERTY_NAME_STR) && !newWidget) {
                return;
            }
            wstringVector vstr = model->findStringVectorProperty(GO_STRING_PROPERTY_NAME_STR);
            std::wstring str = vstr.size() > 0 ? vstr[0] : L"";
            ((QCheckBox*)widget.data())->setText(wstringToQString(str));
            model->clearChanged(GO_STRING_PROPERTY_NAME_STR);
        }
        //=============================================================================
        void
        onStringChangedText(bool newWidget)
        {
            if (!model->stringCheck(GO_STYLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_TEXT_STR)) {
                return;
            }
            if (!model->hasChanged(GO_STRING_PROPERTY_NAME_STR) && !newWidget) {
                return;
            }
            wstringVector vstr = model->findStringVectorProperty(GO_STRING_PROPERTY_NAME_STR);
            std::wstring str = vstr.size() > 0 ? vstr[0] : L"";
            ((QLabel*)widget.data())->setText(wstringToQString(str));
            model->clearChanged(GO_STRING_PROPERTY_NAME_STR);
        }
        //=============================================================================
        void
        onStringChangedEdit(bool newWidget)
        {
            if (!model->stringCheck(GO_STYLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_EDIT_STR)) {
                return;
            }
            if (!model->hasChanged(GO_STRING_PROPERTY_NAME_STR) && !newWidget) {
                return;
            }
            wstringVector vstr = model->findStringVectorProperty(GO_STRING_PROPERTY_NAME_STR);
            if (widget->objectName() == "QMultiLineEditWidget") {
                std::wstring line;
                for (size_t i = 0; i < vstr.size(); ++i) {
                    line += vstr[i];
                    if (i < vstr.size() - 1) {
                        line += L'\n';
                    }
                }
                ((QMultiLineEditWidget*)widget.data())->setPlainText(wstringToQString(line));
            } else {
                std::wstring str = vstr.size() > 0 ? vstr[0] : L"";
                ((QLineEditWidget*)widget.data())->setText(wstringToQString(str));
            }
            model->clearChanged(GO_STRING_PROPERTY_NAME_STR);
        }
        //=============================================================================
        void
        onStringChangedListBox(bool newWidget)
        {
            if (model->stringCheck(GO_STYLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_LISTBOX_STR)
                && (model->hasChanged(GO_STRING_PROPERTY_NAME_STR) || newWidget)) {
                ((QListWidget*)widget.data())->clear();
                ArrayOf value = model->findProperty(GO_STRING_PROPERTY_NAME_STR)->get();
                wstringVector values;
                if (value.isScalarStringArray()) {
                    model->tokenize(value.asWideString(), values, L"|");
                } else if (value.isStringArray()) {
                    values = value.asWideStringVector();
                } else if (value.isCellArrayOfCharacterVectors()) {
                    values = value.asWideStringVector();
                } else if (value.isCharacterArray()) {
                    model->tokenize(value.asWideString(), values, L"|");
                }
                for (size_t k = 0; k < values.size(); k++) {
                    ((QListWidget*)widget.data())->addItem(wstringToQString(values[k]));
                }
                if (values.size() > 0) {
                    model->setScalarDoubleDefault(GO_VALUE_PROPERTY_NAME_STR, 1);
                    ((QListWidget*)widget.data())->setCurrentRow(0);
                }
                model->clearChanged(GO_STRING_PROPERTY_NAME_STR);
            }
        }
        //=============================================================================
        void
        onStringChangedPopupMenu(bool newWidget)
        {
            if (model->stringCheck(GO_STYLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_POPUPMENU_STR)
                && (model->hasChanged(GO_STRING_PROPERTY_NAME_STR) || newWidget)) {
                ((QComboBox*)widget.data())->clear();
                ArrayOf value = model->findProperty(GO_STRING_PROPERTY_NAME_STR)->get();
                wstringVector values;
                if (value.isScalarStringArray()) {
                    model->tokenize(value.asWideString(), values, L"|");
                } else if (value.isStringArray()) {
                    values = value.asWideStringVector();
                } else if (value.isCellArrayOfCharacterVectors()) {
                    values = value.asWideStringVector();
                } else if (value.isCharacterArray()) {
                    model->tokenize(value.asWideString(), values, L"|");
                }
                for (size_t k = 0; k < values.size(); k++) {
                    ((QComboBox*)widget.data())->addItem(wstringToQString(values[k]));
                }
                if (!model->hasChanged(GO_VALUE_PROPERTY_NAME_STR)) {
                    model->findProperty(GO_VALUE_PROPERTY_NAME_STR)->setModified(true);
                }
            }
        }
        //=============================================================================
        void
        onStringPropertyChanged(bool newWidget)
        {
            onStringChangedPushButton(newWidget);
            onStringChangedToggleButton(newWidget);
            onStringChangedRadioButton(newWidget);
            onStringChangedCheckBox(newWidget);
            onStringChangedText(newWidget);
            onStringChangedEdit(newWidget);
            onStringChangedListBox(newWidget);
            onStringChangedPopupMenu(newWidget);
        }
        //=============================================================================
        void
        onParentPositionChanged(bool newWidget)
        {
            bool mustChange = model->getParentFigureHost()->hostFigure()->hasChanged(
                                  GO_POSITION_PROPERTY_NAME_STR)
                || newWidget;
            if (!mustChange) {
                return;
            }
            double width = 0;
            double height = 0;
            parentLayoutSize(width, height);
            std::vector<double> sizevec;
            if (!positionInPixels(sizevec, width, height)) {
                return;
            }
            applyWidgetGeometry(sizevec, height);
        }
        //=============================================================================
        void
        onVisiblePropertyChanged(bool newWidget)
        {
            if (model->hasChanged(GO_VISIBLE_PROPERTY_NAME_STR) && !newWidget) {
                return;
            }
            if (model->stringCheck(GO_VISIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR)) {
                hide();
            } else {
                show();
            }
            model->clearChanged(GO_VISIBLE_PROPERTY_NAME_STR);
        }
        //=============================================================================
        void
        onBackgroundPropertyChanged(bool newWidget)
        {
            if (!model->hasChanged(GO_BACKGROUND_COLOR_PROPERTY_NAME_STR) && !newWidget) {
                return;
            }
            widget->setStyleSheet(buildWidgetStyleSheet(model));
            model->clearChanged(GO_BACKGROUND_COLOR_PROPERTY_NAME_STR);
        }
        //=============================================================================
        void
        onForegroundPropertyChanged(bool newWidget)
        {
            if (!model->hasChanged(GO_FOREGROUND_COLOR_PROPERTY_NAME_STR) && !newWidget) {
                return;
            }
            widget->setStyleSheet(buildWidgetStyleSheet(model));
            model->clearChanged(GO_FOREGROUND_COLOR_PROPERTY_NAME_STR);
        }
        //=============================================================================
        void
        onFontNameChanged(bool newWidget)
        {
            if (!model->hasChanged(GO_FONT_NAME_PROPERTY_NAME_STR) && !newWidget) {
                return;
            }

            QFont font = widget->font();
            font.setStyleStrategy(QFont::PreferAntialias);
            widget->setFont(font);

            GOStringProperty* fontname = static_cast<GOStringProperty*>(
                model->findProperty(GO_FONT_NAME_PROPERTY_NAME_STR));
            if (fontname) {
                widget->setStyleSheet(buildWidgetStyleSheet(model));
            }
            model->clearChanged(GO_FONT_NAME_PROPERTY_NAME_STR);
        }
        //=============================================================================
        void
        onFontSizeOrUnitChanged(bool newWidget)
        {
            if (!model->hasChanged(GO_FONT_SIZE_PROPERTY_NAME_STR)
                && !model->hasChanged(GO_FONT_UNITS_PROPERTY_NAME_STR) && !newWidget) {
                return;
            }

            GOScalarProperty* fSize = static_cast<GOScalarProperty*>(
                model->findProperty(GO_FONT_SIZE_PROPERTY_NAME_STR));
            if (fSize) {
                widget->setStyleSheet(buildWidgetStyleSheet(model));
            }
            model->clearChanged(GO_FONT_SIZE_PROPERTY_NAME_STR);
            model->clearChanged(GO_FONT_UNITS_PROPERTY_NAME_STR);
        }
        //=============================================================================
        void
        onFontAngleChanged(bool newWidget)
        {
            if (!model->hasChanged(GO_FONT_ANGLE_PROPERTY_NAME_STR) && !newWidget) {
                return;
            }
            GOFontAngleProperty* fontangle
                = (GOFontAngleProperty*)model->findProperty(GO_FONT_ANGLE_PROPERTY_NAME_STR);
            if (fontangle) {
                widget->setStyleSheet(buildWidgetStyleSheet(model));
            }
            model->clearChanged(GO_FONT_ANGLE_PROPERTY_NAME_STR);
        }
        //=============================================================================
        void
        onFontWeightChanged(bool newWidget)
        {
            if (!model->hasChanged(GO_FONT_WEIGHT_PROPERTY_NAME_STR) && !newWidget) {
                return;
            }
            GOFontWeightProperty* fontweight
                = (GOFontWeightProperty*)model->findProperty(GO_FONT_WEIGHT_PROPERTY_NAME_STR);
            if (fontweight) {
                widget->setStyleSheet(buildWidgetStyleSheet(model));
            }
            model->clearChanged(GO_FONT_WEIGHT_PROPERTY_NAME_STR);
        }
        //=============================================================================
        void
        onHAlignmentChanged(bool newWidget)
        {
            if (!model->hasChanged(GO_HORIZONTAL_ALIGNMENT_PROPERTY_NAME_STR) && !newWidget) {
                return;
            }

            if (model->stringCheck(GO_STYLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_TEXT_STR)
                || model->stringCheck(GO_STYLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_EDIT_STR)) {
                GOAlignHorizProperty* textAlign = static_cast<GOAlignHorizProperty*>(
                    model->findProperty(GO_HORIZONTAL_ALIGNMENT_PROPERTY_NAME_STR));
                if (textAlign) {
                    widget->setStyleSheet(buildWidgetStyleSheet(model));
                }
                model->clearChanged(GO_HORIZONTAL_ALIGNMENT_PROPERTY_NAME_STR);
            }
        }
        //=============================================================================
        void
        onEnableChanged(bool newWidget)
        {
            if (!model->hasChanged(GO_ENABLE_PROPERTY_NAME_STR) && !newWidget) {
                return;
            }
            if (widget) {
                const bool inactive = model->isEnableInactive();
                const bool on
                    = model->stringCheck(GO_ENABLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
                // 'inactive' keeps the widget enabled so it renders normally
                // (not greyed) and still receives the mouse press that fires
                // ButtonDownFcn; the interaction itself is suppressed in the
                // widget event handlers (UIControl*Widget) and in the model's
                // clicked(). Editable widgets are additionally set read-only so
                // keyboard input cannot change their content.
                widget->setEnabled(on || inactive);
                if (model->stringCheck(GO_STYLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_EDIT_STR)) {
                    if (widget->objectName() == "QMultiLineEditWidget") {
                        static_cast<QMultiLineEdit*>(widget.data())->setReadOnly(inactive);
                    } else {
                        static_cast<QLineEdit*>(widget.data())->setReadOnly(inactive);
                    }
                }
            }
            model->clearChanged(GO_ENABLE_PROPERTY_NAME_STR);
        }
        //=============================================================================
        void
        onToolTipsChanged(bool newWidget)
        {
            if (model->hasChanged(GO_TOOLTIP_PROPERTY_NAME_STR) || newWidget) {
                std::wstring str = model->findStringProperty(GO_TOOLTIP_PROPERTY_NAME_STR);
                widget->setToolTip(wstringToQString(str));
                model->clearChanged(GO_TOOLTIP_PROPERTY_NAME_STR);
            }
        }
        //=============================================================================
        void
        onRadioButtonChanged(bool newWidget)
        {
            if (model->stringCheck(GO_STYLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_RADIOBUTTON_STR)
                && (model->hasChanged(GO_VALUE_PROPERTY_NAME_STR)
                    || model->hasChanged(GO_MIN_PROPERTY_NAME_STR)
                    || model->hasChanged(GO_MAX_PROPERTY_NAME_STR) || newWidget)) {
                double min(model->findScalarDoubleProperty(GO_MIN_PROPERTY_NAME_STR));
                double max(model->findScalarDoubleProperty(GO_MAX_PROPERTY_NAME_STR));
                double value(model->findScalarDoubleProperty(GO_VALUE_PROPERTY_NAME_STR));
                if (value == min) {
                    ((QRadioButton*)widget.data())->setChecked(false);
                } else if (value == max) {
                    ((QRadioButton*)widget.data())->setChecked(true);
                }
                model->clearChanged(GO_MIN_PROPERTY_NAME_STR);
                model->clearChanged(GO_MAX_PROPERTY_NAME_STR);
                model->clearChanged(GO_VALUE_PROPERTY_NAME_STR);
            }
        }
        //=============================================================================
        void
        onCheckBoxChanged(bool newWidget)
        {
            if (model->stringCheck(GO_STYLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_CHECKBOX_STR)
                && (model->hasChanged(GO_VALUE_PROPERTY_NAME_STR)
                    || model->hasChanged(GO_MIN_PROPERTY_NAME_STR)
                    || model->hasChanged(GO_MAX_PROPERTY_NAME_STR) || newWidget)) {
                double min(model->findScalarDoubleProperty(GO_MIN_PROPERTY_NAME_STR));
                double max(model->findScalarDoubleProperty(GO_MAX_PROPERTY_NAME_STR));
                double value(model->findScalarDoubleProperty(GO_VALUE_PROPERTY_NAME_STR));
                if (value == min) {
                    ((QCheckBox*)widget.data())->setChecked(false);
                } else if (value == max) {
                    ((QCheckBox*)widget.data())->setChecked(true);
                }
                model->clearChanged(GO_MIN_PROPERTY_NAME_STR);
                model->clearChanged(GO_MAX_PROPERTY_NAME_STR);
                model->clearChanged(GO_VALUE_PROPERTY_NAME_STR);
            }
        }
        //=============================================================================
        void
        onSliderChanged(bool newWidget)
        {
            if (model->stringCheck(GO_STYLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_SLIDER_STR)
                && (model->hasChanged(GO_MIN_PROPERTY_NAME_STR)
                    || model->hasChanged(GO_MAX_PROPERTY_NAME_STR)
                    || model->hasChanged(GO_SLIDER_STEP_PROPERTY_NAME_STR) || newWidget)) {
                double min(model->findScalarDoubleProperty(GO_MIN_PROPERTY_NAME_STR));
                double max(model->findScalarDoubleProperty(GO_MAX_PROPERTY_NAME_STR));
                std::vector<double> steps(
                    model->findVectorDoubleProperty(GO_SLIDER_STEP_PROPERTY_NAME_STR));
                ((QSlider*)widget.data())->setMinimum(0);
                ((QSlider*)widget.data())->setMaximum((int)((max - min) / steps[0]));
                ((QSlider*)widget.data())->setSingleStep(1);
                // SliderStep(2) is the page increment in the same units as SliderStep(1), so the
                // page step in integer positions is their ratio. Truncating steps[1] alone turned
                // the default 0.10 into 0 and killed trough/PageUp-PageDown paging. Keep at least
                // one step.
                int sliderPageStep = (int)(steps[1] / steps[0] + 0.5);
                if (sliderPageStep < 1) {
                    sliderPageStep = 1;
                }
                ((QSlider*)widget.data())->setPageStep(sliderPageStep);
                if (newWidget) {
                    if (model->hasChanged(GO_VALUE_PROPERTY_NAME_STR)) {
                        double value(model->findScalarDoubleProperty(GO_VALUE_PROPERTY_NAME_STR));
                        ((QSlider*)widget.data())->setValue((int)((value - min) / steps[0]));
                        model->clearChanged(GO_VALUE_PROPERTY_NAME_STR);
                    } else {
                        model->findProperty(GO_VALUE_PROPERTY_NAME_STR)
                            ->set(ArrayOf::doubleScalar(
                                min + steps[0] * ((QSlider*)widget.data())->value()));
                    }
                } else {
                    model->findProperty(GO_VALUE_PROPERTY_NAME_STR)
                        ->set(ArrayOf::doubleScalar(
                            min + steps[0] * ((QSlider*)widget.data())->value()));
                }
                model->clearChanged(GO_MIN_PROPERTY_NAME_STR);
                model->clearChanged(GO_MAX_PROPERTY_NAME_STR);
                model->clearChanged(GO_SLIDER_STEP_PROPERTY_NAME_STR);
            }
        }
        //=============================================================================
        void
        onToggleButtonChanged(bool newWidget)
        {
            if (model->stringCheck(GO_STYLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_TOGGLEBUTTON_STR)
                && (model->hasChanged(GO_VALUE_PROPERTY_NAME_STR)
                    || model->hasChanged(GO_MIN_PROPERTY_NAME_STR)
                    || model->hasChanged(GO_MAX_PROPERTY_NAME_STR) || newWidget)) {
                double min(model->findScalarDoubleProperty(GO_MIN_PROPERTY_NAME_STR));
                double max(model->findScalarDoubleProperty(GO_MAX_PROPERTY_NAME_STR));
                double value(model->findScalarDoubleProperty(GO_VALUE_PROPERTY_NAME_STR));
                if (value == min) {
                    ((QPushButton*)widget.data())->setChecked(false);
                } else if (value == max) {
                    ((QPushButton*)widget.data())->setChecked(true);
                }
                model->clearChanged(GO_MIN_PROPERTY_NAME_STR);
                model->clearChanged(GO_MAX_PROPERTY_NAME_STR);
                model->clearChanged(GO_VALUE_PROPERTY_NAME_STR);
            }
        }
        //=============================================================================
        void
        onEditPropertyChanged(bool newWidget)
        {
            if (model->stringCheck(GO_STYLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_EDIT_STR)
                && (model->hasChanged(GO_STRING_PROPERTY_NAME_STR)
                    || model->hasChanged(GO_MIN_PROPERTY_NAME_STR)
                    || model->hasChanged(GO_MAX_PROPERTY_NAME_STR))) {
                // Min/Max flip between single- and multi-line edit widgets: recreate the widget
                // through the model.
                destroy();
                model->clearChanged(GO_MIN_PROPERTY_NAME_STR);
                model->clearChanged(GO_MAX_PROPERTY_NAME_STR);
                model->clearChanged(GO_STRING_PROPERTY_NAME_STR);
                model->findProperty(GO_STYLE_PROPERTY_NAME_STR)->setModified(true);
                model->updateState();
            }
        }
        //=============================================================================
        void
        onCDataPropertyChanged(bool newWidget)
        {
            if (!model->stringCheck(GO_STYLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_PUSHBUTTON_STR)) {
                return;
            }
            if (newWidget || model->hasChanged(GO_C_DATA_PROPERTY_NAME_STR)) {
                ArrayOf cdata(model->findArrayOfProperty(GO_C_DATA_PROPERTY_NAME_STR));
                if (cdata.isEmpty()) {
                    QPushButtonWidget* button = static_cast<QPushButtonWidget*>(widget.data());
                    QImage img;
                    button->setImage(img);
                    model->clearChanged(GO_C_DATA_PROPERTY_NAME_STR);
                    return;
                }
                bool isValid = (cdata.dimensions().length() == 3) && (cdata.dimensionLength(2) == 3)
                    && cdata.isNumeric();
                if (!isValid) {
                    Error("Nelson:uicontrol:invalidImageFormat",
                        "Invalid image format: expected a 3D array with 3 color channels.");
                }

                QPushButtonWidget* button = static_cast<QPushButtonWidget*>(widget.data());
                QImage img;
                if (cdata.dataClass() == NLS_UINT8) {
                    bool isValid
                        = (cdata.dimensions().length() == 3) && (cdata.dimensionLength(2) == 3);
                    if (!isValid) {
                        Error("Nelson:uicontrol:invalidImageFormat",
                            "Invalid image format: expected a 3D array with 3 color channels.");
                    }
                    indexType rows = cdata.rows();
                    indexType cols = cdata.columns();
                    uint8* dp = (uint8*)cdata.dataPointer();
                    img = QImage((int)cols, (int)rows, QImage::Format_ARGB32);
                    for (indexType i = 0; i < rows; i++) {
                        QRgb* ibits = (QRgb*)img.scanLine((int)i);
                        for (indexType j = 0; j < cols; j++) {
                            int r = (int)(dp[(i + j * rows)]);
                            int g = (int)(dp[(i + j * rows) + rows * cols]);
                            int b = (int)(dp[(i + j * rows) + 2 * rows * cols]);
                            int a = (int)(255);
                            ibits[j] = qRgba(r, g, b, a);
                        }
                    }
                } else {
                    cdata.promoteType(NLS_DOUBLE);
                    double* dp = (double*)cdata.dataPointer();
                    bool isValid = true;
                    for (indexType k = 0; k < cdata.elementCount(); ++k) {
                        if (dp[k] < 0.0 || dp[k] > 1.0) {
                            isValid = false;
                            break;
                        }
                    }
                    if (!isValid) {
                        Error("Nelson:uicontrol:imageValuesOutOfRange",
                            "Invalid image format: array with values in [0. 1.] expected.");
                    }
                    indexType rows = cdata.rows();
                    indexType cols = cdata.columns();

                    img = QImage((int)cols, (int)rows, QImage::Format_ARGB32);
                    for (indexType i = 0; i < rows; i++) {
                        QRgb* ibits = (QRgb*)img.scanLine((int)i);
                        for (indexType j = 0; j < cols; j++) {
                            int r = (int)(dp[(i + j * rows)] * 255);
                            int g = (int)(dp[(i + j * rows) + rows * cols] * 255);
                            int b = (int)(dp[(i + j * rows) + 2 * rows * cols] * 255);
                            int a = 255;
                            ibits[j] = qRgba(r, g, b, a);
                        }
                    }
                }
                model->clearChanged(GO_C_DATA_PROPERTY_NAME_STR);
                button->setImage(img);
                button->repaint();
            }
        }
        //=============================================================================
        void
        onListTopBoxChanged(bool newWidget)
        {
            if (!model->stringCheck(GO_STYLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_LISTBOX_STR)) {
                return;
            }
            if (model->hasChanged(GO_STRING_PROPERTY_NAME_STR)) {
                ((QListWidget*)widget.data())->clear();
                ArrayOf value = model->findProperty(GO_STRING_PROPERTY_NAME_STR)->get();
                wstringVector values;
                if (value.isScalarStringArray()) {
                    model->tokenize(value.asWideString(), values, L"|");
                } else if (value.isStringArray()) {
                    values = value.asWideStringVector();
                } else if (value.isCellArrayOfCharacterVectors()) {
                    values = value.asWideStringVector();
                } else if (value.isCharacterArray()) {
                    model->tokenize(value.asWideString(), values, L"|");
                }
                for (size_t k = 0; k < values.size(); k++) {
                    ((QListWidget*)widget.data())->addItem(wstringToQString(values[k]));
                }
                if (values.size() > 0) {
                    ((QListWidget*)widget.data())->setCurrentRow((int)1);
                }
                if (!model->hasChanged(GO_VALUE_PROPERTY_NAME_STR) && !values.empty()
                    && model->findScalarDoubleProperty(GO_VALUE_PROPERTY_NAME_STR) == 0) {
                    model->setScalarDoubleDefault(GO_VALUE_PROPERTY_NAME_STR, 1);
                }
            }

            if (model->hasChanged(GO_MIN_PROPERTY_NAME_STR)
                || model->hasChanged(GO_MAX_PROPERTY_NAME_STR) || newWidget) {
                double min(model->findScalarDoubleProperty(GO_MIN_PROPERTY_NAME_STR));
                double max(model->findScalarDoubleProperty(GO_MAX_PROPERTY_NAME_STR));
                if (max - min > 1) {
                    ((QListWidget*)widget.data())
                        ->setSelectionMode(QAbstractItemView::MultiSelection);
                } else {

                    ((QListWidget*)widget.data())
                        ->setSelectionMode(QAbstractItemView::SingleSelection);
                }
                model->clearChanged(GO_MIN_PROPERTY_NAME_STR);
                model->clearChanged(GO_MAX_PROPERTY_NAME_STR);
            }
            if (model->hasChanged(GO_LISTBOX_TOP_PROPERTY_NAME_STR) || newWidget) {
                int listTop((int)model->findScalarDoubleProperty(GO_LISTBOX_TOP_PROPERTY_NAME_STR));
                ((QListWidget*)widget.data())
                    ->scrollToItem(((QListWidget*)widget.data())->item(listTop - 1),
                        QAbstractItemView::PositionAtTop);
                model->clearChanged(GO_LISTBOX_TOP_PROPERTY_NAME_STR);
            }

            if (model->hasChanged(GO_VALUE_PROPERTY_NAME_STR)) {
                double v = model->findScalarDoubleProperty(GO_VALUE_PROPERTY_NAME_STR);
                ((QListWidget*)widget.data())->setCurrentRow((int)v - 1);

                model->clearChanged(GO_VALUE_PROPERTY_NAME_STR);
            }
        }
        //=============================================================================
        void
        onPopupMenuChanged(bool newWidget)
        {
            if (model->stringCheck(GO_STYLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_POPUPMENU_STR)) {
                ((QComboBox*)widget.data())->clear();
                ArrayOf value = model->findProperty(GO_STRING_PROPERTY_NAME_STR)->get();
                wstringVector values;
                if (value.isScalarStringArray()) {
                    model->tokenize(value.asWideString(), values, L"|");
                } else if (value.isStringArray()) {
                    values = value.asWideStringVector();
                } else if (value.isCellArrayOfCharacterVectors()) {
                    values = value.asWideStringVector();
                } else if (value.isCharacterArray()) {
                    model->tokenize(value.asWideString(), values, L"|");
                }
                for (size_t k = 0; k < values.size(); k++) {
                    ((QComboBox*)widget.data())->addItem(wstringToQString(values[k]));
                }

                double indexPosition = model->findScalarDoubleProperty(GO_VALUE_PROPERTY_NAME_STR);

                if (!values.empty() && indexPosition == 0) {
                    model->setScalarDoubleDefault(GO_VALUE_PROPERTY_NAME_STR, 1);
                    indexPosition = 1;
                }
                ((QComboBox*)widget.data())->setCurrentIndex((int)(indexPosition - 1));
            }
        }
        //=============================================================================
        void
        onValueChanged(bool newWidget)
        {
            if (!widget) {
                return;
            }
            if (model->hasChanged(GO_VALUE_PROPERTY_NAME_STR) || newWidget) {
                if (widget->objectName() == "QSliderWidget") {
                    double min(model->findScalarDoubleProperty(GO_MIN_PROPERTY_NAME_STR));
                    std::vector<double> steps(
                        model->findVectorDoubleProperty(GO_SLIDER_STEP_PROPERTY_NAME_STR));
                    double value(model->findScalarDoubleProperty(GO_VALUE_PROPERTY_NAME_STR));
                    ((QSlider*)widget.data())->setValue((int)((value - min) / steps[0]));
                }
                if (widget->objectName() == "QListBoxWidget") {
                    double value(model->findScalarDoubleProperty(GO_VALUE_PROPERTY_NAME_STR));
                    ((QListWidget*)widget.data())->setCurrentRow((int)value - 1);
                }

                model->clearChanged(GO_VALUE_PROPERTY_NAME_STR);
            }
        }
        //=============================================================================
        void
        onPositionPropertyChanged(bool newWidget)
        {
            bool needContinue = model->hasChanged(GO_POSITION_PROPERTY_NAME_STR) || newWidget;
            if (!needContinue) {
                return;
            }
            double width = 0;
            double height = 0;
            parentLayoutSize(width, height);
            std::vector<double> sizevec;
            if (!positionInPixels(sizevec, width, height)) {
                return;
            }

            applyWidgetGeometry(sizevec, height);
            model->clearChanged(GO_POSITION_PROPERTY_NAME_STR);
        }
        //=============================================================================
    };
    //=============================================================================
} // namespace
//=============================================================================
UIComponentBinding*
createUIControlBinding(GOUIControl* control)
{
    return new UIControlBinding(control);
}
//=============================================================================
static wstringVector
getModifiers(QKeyEvent* keyEvent)
{
    wstringVector modifiersWstringVector;
    modifiersWstringVector.reserve(4);
    Qt::KeyboardModifiers modifiers = keyEvent->modifiers();
    if (modifiers & Qt::ShiftModifier) {
        modifiersWstringVector.push_back(L"shift");
    }
    if (modifiers & Qt::ControlModifier) {
        modifiersWstringVector.push_back(L"control");
    }
    if (modifiers & Qt::AltModifier) {
        modifiersWstringVector.push_back(L"alt");
    }
    if (modifiers & Qt::MetaModifier) {
        modifiersWstringVector.push_back(L"meta");
    }
    return modifiersWstringVector;
}
//=============================================================================
bool
UIControlHandleKeyEvent(GOUIControl* control, QEvent* event,
    const std::wstring& callbackPropertyStr, const std::wstring& eventType)
{
    GOCallbackProperty* goCallback
        = (GOCallbackProperty*)control->findProperty(callbackPropertyStr);
    ArrayOf callbackArrayOf = goCallback->get();
    if (callbackArrayOf.isEmpty()) {
        return true;
    }

    QKeyEvent* keyEvent = static_cast<QKeyEvent*>(event);
    wstringVector modifiersWstringVector = getModifiers(keyEvent);
    std::wstring key = QStringTowstring(QKeySequence(keyEvent->key()).toString());
    std::wstring character = QStringTowstring(keyEvent->text());

    goCallback->pushKeyEvent(
        control, L"UIClientComponentKeyEvent", eventType, character, key, modifiersWstringVector);
    return true;
}
//=============================================================================
bool
UIControlHandleMouseEvent(GOUIControl* control, QEvent* event,
    const std::wstring& callbackPropertyStr, const std::wstring& eventType)
{
    GOCallbackProperty* goCallback
        = (GOCallbackProperty*)control->findProperty(callbackPropertyStr);
    ArrayOf callbackArrayOf = goCallback->get();
    if (callbackArrayOf.isEmpty()) {
        return true;
    }
    int64 thisHandle = findGraphicsObjectHandle(control);
    if (thisHandle != -1) {
        setCurrentObject(thisHandle);
    }
    goCallback->pushEvent(control, L"MouseData", eventType);
    return true;
}
//=============================================================================
// Style-dispatched widget read-back: sync Value/String from the widget before the
// model queues the Callback.
void
UIControlSyncValueFromWidget(GOUIControl* control, QWidget* widget)
{
    if (!control || !widget) {
        return;
    }
    if (control->stringCheck(GO_STYLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_TOGGLEBUTTON_STR)) {
        if (((QPushButton*)widget)->isChecked()) {
            control->findProperty(GO_VALUE_PROPERTY_NAME_STR)
                ->set(ArrayOf::doubleScalar(
                    control->findScalarDoubleProperty(GO_MAX_PROPERTY_NAME_STR)));
        } else {
            control->findProperty(GO_VALUE_PROPERTY_NAME_STR)
                ->set(ArrayOf::doubleScalar(
                    control->findScalarDoubleProperty(GO_MIN_PROPERTY_NAME_STR)));
        }
        return;
    }
    if (control->stringCheck(GO_STYLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_SLIDER_STR)) {
        double min(control->findScalarDoubleProperty(GO_MIN_PROPERTY_NAME_STR));
        std::vector<double> steps(
            control->findVectorDoubleProperty(GO_SLIDER_STEP_PROPERTY_NAME_STR));
        control->findProperty(GO_VALUE_PROPERTY_NAME_STR)
            ->set(ArrayOf::doubleScalar(min + steps[0] * ((QSlider*)widget)->value()));
        control->clearChanged(GO_VALUE_PROPERTY_NAME_STR);
        return;
    }
    if (control->stringCheck(GO_STYLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_RADIOBUTTON_STR)) {
        if (((QRadioButton*)widget)->isChecked()) {
            control->findProperty(GO_VALUE_PROPERTY_NAME_STR)
                ->set(control->findProperty(GO_MAX_PROPERTY_NAME_STR)->get());
        } else {
            control->findProperty(GO_VALUE_PROPERTY_NAME_STR)
                ->set(control->findProperty(GO_MIN_PROPERTY_NAME_STR)->get());
        }
        return;
    }
    if (control->stringCheck(GO_STYLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_CHECKBOX_STR)) {
        if (((QCheckBox*)widget)->isChecked()) {
            control->findProperty(GO_VALUE_PROPERTY_NAME_STR)
                ->set(control->findProperty(GO_MAX_PROPERTY_NAME_STR)->get());
        } else {
            control->findProperty(GO_VALUE_PROPERTY_NAME_STR)
                ->set(control->findProperty(GO_MIN_PROPERTY_NAME_STR)->get());
        }
        control->findProperty(GO_VALUE_PROPERTY_NAME_STR)->setModified(true);
        return;
    }
    if (control->stringCheck(GO_STYLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_EDIT_STR)) {
        QString qstr;
        if (widget->objectName() == "QMultiLineEditWidget") {
            qstr = ((QMultiLineEdit*)widget)->toPlainText();
        } else {
            qstr = ((QLineEdit*)widget)->text();
        }
        control->setStringDefault(GO_STRING_PROPERTY_NAME_STR, QStringTowstring(qstr));
        return;
    }
    if (control->stringCheck(GO_STYLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_LISTBOX_STR)) {
        QList<QListWidgetItem*> selection(((QListWidget*)widget)->selectedItems());
        std::vector<double> selected;
        for (int i = 0; i < selection.size(); i++) {
            selected.push_back(((QListWidget*)widget)->row(selection[i]) + 1);
        }
        ((GOVectorProperty*)control->findProperty(GO_VALUE_PROPERTY_NAME_STR))->data(selected);
        control->clearChanged(GO_VALUE_PROPERTY_NAME_STR);
        return;
    }
    if (control->stringCheck(GO_STYLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_POPUPMENU_STR)) {
        int value = ((QComboBox*)widget)->currentIndex() + 1;
        control->findProperty(GO_VALUE_PROPERTY_NAME_STR)->set(ArrayOf::doubleScalar(value));
        control->clearChanged(GO_VALUE_PROPERTY_NAME_STR);
        return;
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
