//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <QtWidgets/QWidget>
#include <QtGui/QPainter>
#include <QtGui/QPaintEvent>
#include "UIComponentBindings.hpp"
#include "UIComponentBindingBase.hpp"
#include "GOUIPanel.h"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "GOColorProperty.hpp"
#include "GOHelpers.hpp"
#include "RenderQt.hpp"
#include "QStringConverter.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
namespace {
    //=============================================================================
    class QUIPanelWidget : public QWidget
    {
        GOUIPanel* goPanel;

    public:
        QColor backgroundColor { 245, 245, 245 };
        QColor borderColor { 125, 125, 125 };
        QColor titleColor { 33, 33, 33 };
        int borderWidth = 1;
        bool borderVisible = true;
        QString title;
        int titleAlignment = 0; // 0: left, 1: center, 2: right
        //=============================================================================
        QUIPanelWidget(QWidget* parent, GOUIPanel* owner) : QWidget(parent), goPanel(owner)
        {
            setProperty("nelsonPaintsGraphicsChildren", true);
        }
        //=============================================================================
        int
        titleHeight() const
        {
            return title.isEmpty() ? 0 : fontMetrics().height() + 4;
        }
        //=============================================================================
    protected:
        void
        paintEvent(QPaintEvent* /*event*/) override
        {
            // App-style look: the border encloses the whole panel including the
            // title band; a separator line closes the title band
            QPainter painter(this);
            painter.fillRect(rect(), backgroundColor);
            int textHeight = titleHeight();
            if (!title.isEmpty()) {
                painter.setPen(titleColor);
                painter.setFont(font());
                Qt::Alignment alignment = Qt::AlignVCenter;
                switch (titleAlignment) {
                case 1:
                    alignment |= Qt::AlignHCenter;
                    break;
                case 2:
                    alignment |= Qt::AlignRight;
                    break;
                default:
                    alignment |= Qt::AlignLeft;
                    break;
                }
                QRect titleRect(
                    borderWidth + 4, borderWidth, width() - 2 * (borderWidth + 4), textHeight);
                painter.drawText(titleRect, alignment, title);
            }
            if (borderVisible && borderWidth > 0) {
                QPen pen(borderColor);
                pen.setWidth(borderWidth);
                painter.setPen(pen);
                int half = borderWidth / 2;
                painter.drawRect(half, half, width() - borderWidth - 1, height() - borderWidth - 1);
                if (!title.isEmpty()) {
                    painter.drawLine(
                        0, borderWidth + textHeight, width() - 1, borderWidth + textHeight);
                }
            }
            painter.end();
            paintGraphicsChildren();
        }
        //=============================================================================
        // The panel covers the figure canvas, so the axes and other graphics
        // objects it holds have to be drawn here: nothing else paints them.
        void
        paintGraphicsChildren()
        {
            if (!goPanel || width() <= 0 || height() <= 0) {
                return;
            }
            QPainter painter(this);
            painter.setRenderHint(QPainter::Antialiasing);
            painter.setRenderHint(QPainter::TextAntialiasing);
            RenderQt renderer(&painter, 0, 0, width(), height(), L"Image");
            goPanel->paintContentInFrame(renderer);
        }
        //=============================================================================
    };
    //=============================================================================
    static QColor
    colorFromProperty(GraphicsObject* go, const std::wstring& name, const QColor& fallback)
    {
        GOColorProperty* colorProperty
            = static_cast<GOColorProperty*>(go->findProperty(name, false));
        if (!colorProperty || colorProperty->isNone()) {
            return fallback;
        }
        std::vector<double> rgb = colorProperty->data();
        if (rgb.size() < 3) {
            return fallback;
        }
        return QColor::fromRgb(
            (int)(rgb[0] * 255.0 + 0.5), (int)(rgb[1] * 255.0 + 0.5), (int)(rgb[2] * 255.0 + 0.5));
    }
    //=============================================================================
    class UIPanelBinding : public QtUIComponentBinding
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
            widget = new QUIPanelWidget(parent, static_cast<GOUIPanel*>(model));
        }
        //=============================================================================
        void
        applyStateToWidget(bool isNewWidget) override
        {
            QUIPanelWidget* panel = static_cast<QUIPanelWidget*>(widget.data());
            if (!panel) {
                return;
            }
            if (isNewWidget || model->hasChanged(GO_TITLE_PROPERTY_NAME_STR)
                || model->hasChanged(GO_TITLE_POSITION_PROPERTY_NAME_STR)
                || model->hasChanged(GO_BORDER_TYPE_PROPERTY_NAME_STR)
                || model->hasChanged(GO_BORDER_WIDTH_PROPERTY_NAME_STR)
                || model->hasChanged(GO_BORDER_COLOR_PROPERTY_NAME_STR)
                || model->hasChanged(GO_FOREGROUND_COLOR_PROPERTY_NAME_STR)
                || model->hasChanged(GO_BACKGROUND_COLOR_PROPERTY_NAME_STR)) {
                panel->title
                    = wstringToQString(model->findStringProperty(GO_TITLE_PROPERTY_NAME_STR));
                std::wstring titlePosition
                    = model->findStringProperty(GO_TITLE_POSITION_PROPERTY_NAME_STR);
                panel->titleAlignment = (titlePosition == L"centertop") ? 1
                    : (titlePosition == L"righttop")                    ? 2
                                                                        : 0;
                panel->borderVisible = model->stringCheck(
                    GO_BORDER_TYPE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_LINE_STR);
                panel->borderWidth
                    = (int)model->findScalarDoubleProperty(GO_BORDER_WIDTH_PROPERTY_NAME_STR);
                panel->backgroundColor = colorFromProperty(
                    model, GO_BACKGROUND_COLOR_PROPERTY_NAME_STR, QColor(245, 245, 245));
                panel->borderColor = colorFromProperty(
                    model, GO_BORDER_COLOR_PROPERTY_NAME_STR, QColor(125, 125, 125));
                panel->titleColor = colorFromProperty(
                    model, GO_FOREGROUND_COLOR_PROPERTY_NAME_STR, QColor(33, 33, 33));
                panel->update();
                // InnerPosition: area inside borders and title
                std::vector<double> position(
                    model->findVectorDoubleProperty(GO_POSITION_PROPERTY_NAME_STR));
                if (position.size() == 4) {
                    double borderInset = panel->borderVisible ? (double)panel->borderWidth : 0.0;
                    double titleInset = (double)panel->titleHeight();
                    model->setFourVectorDefault(GO_INNER_POSITION_PROPERTY_NAME_STR,
                        position[0] + borderInset, position[1] + borderInset,
                        position[2] - 2 * borderInset, position[3] - 2 * borderInset - titleInset);
                }
            }
        }
        //=============================================================================
        void
        applyColorsToWidget() override
        {
            // colors are painted by QUIPanelWidget; do not install a style sheet
            // (it would also restyle every child widget).
        }
        //=============================================================================
    };
    //=============================================================================
} // namespace
//=============================================================================
UIComponentBinding*
createUIPanelBinding(GOUIComponent* component)
{
    return new UIPanelBinding(component);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
