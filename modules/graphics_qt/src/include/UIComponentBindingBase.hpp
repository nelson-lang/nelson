//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include <QtCore/QMargins>
#include <QtCore/QPointer>
#include <QtWidgets/QWidget>
#include "GOUIWidgetBinding.hpp"
#include "GOUIComponent.h"
//=============================================================================
namespace Nelson {
//=============================================================================
// Widget hosting the children of the ui container holding `object`, or the
// figure canvas of `host` when `object` is held by the figure.
QWidget*
uiParentQWidget(GraphicsObject* object, GOFigureHost* host);
//=============================================================================
// Qt implementation chassis of UIComponentBinding: owns the QWidget backing
// one GOUIComponent and carries the property-->widget code shared by all App
// Designer components (position, visibility, fonts, style-sheet colors, the
// raster snapshot used by export paths). Per-component subclasses implement
// createWidget()/applyStateToWidget() with the widget-specific code.
class QtUIComponentBinding : public UIComponentBinding
{
protected:
    GOUIComponent* model;
    QPointer<QWidget> widget;
    QSize lastAppliedSize;

public:
    explicit QtUIComponentBinding(GOUIComponent* component) : model(component) { }
    ~QtUIComponentBinding() override;
    //=============================================================================
    bool
    create() override;
    bool
    exists() const override
    {
        return !widget.isNull();
    }
    //=============================================================================
    void
    applyCommonProperties(bool isNewWidget) override;
    void
    applyState(bool isNewWidget) override
    {
        applyStateToWidget(isNewWidget);
    }
    //=============================================================================
    void
    show() override;
    void
    hide() override;
    void
    setFocus() override;
    void
    destroy() override;
    //=============================================================================
    void*
    nativeWidget() override
    {
        return widget.data();
    }
    //=============================================================================
    bool
    consumeGeometryChange() override;
    //=============================================================================
    void
    renderSnapshot(RenderInterface& gc) override;
    //=============================================================================
protected:
    //=============================================================================
    virtual void
    createWidget()
        = 0;
    virtual void
    applyStateToWidget(bool isNewWidget)
        = 0;
    // extra component specific style sheet rules appended to the common ones
    virtual QString
    additionalStyleSheet()
    {
        return {};
    }
    // extra Qt paint area around the public Position rectangle
    virtual QMargins
    widgetGeometryMargins()
    {
        return {};
    }
    virtual void
    applyParentToWidget();
    //=============================================================================
    QWidget*
    getParentQWidget();
    //=============================================================================
    void
    applyPositionToWidget();
    void
    applyFontToWidget();
    // font described by the FontName/FontSize/FontWeight/FontAngle properties
    QFont
    modelFont();
    virtual void
    applyColorsToWidget();
    void
    getParentLayoutSize(QWidget* parentWidget, double& width, double& height);
    void
    getParentContentOffset(double& left, double& top);
    //=============================================================================
};
//=============================================================================
} // namespace Nelson
//=============================================================================
