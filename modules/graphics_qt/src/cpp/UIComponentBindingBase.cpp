//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <QtGui/QFont>
#include <QtGui/QImage>
#include <QtGui/QPainter>
#include "UIComponentBindingBase.hpp"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "GOColorProperty.hpp"
#include "GOUnitsProperty.hpp"
#include "GOGObjectsProperty.hpp"
#include "GOHelpers.hpp"
#include "GOFigure.hpp"
#include "GORoot.hpp"
#include "GOUIContainer.h"
#include "GOUIComponentUnits.hpp"
#include "RenderInterface.hpp"
#include "RenderQtHelpers.hpp"
#include "UnitsConversionHelpers.hpp"
#include "QStringConverter.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
QtUIComponentBinding::~QtUIComponentBinding() { destroy(); }
//=============================================================================
void
QtUIComponentBinding::getParentLayoutSize(QWidget* parentWidget, double& width, double& height)
{
    width = parentWidget ? parentWidget->width() : 0;
    height = parentWidget ? parentWidget->height() : 0;
    double logicalWidth = 0;
    double logicalHeight = 0;
    if (uiComponentParentSizeInPixels(model, logicalWidth, logicalHeight)) {
        width = logicalWidth;
        height = logicalHeight;
    }
}
//=============================================================================
void
QtUIComponentBinding::getParentContentOffset(double& left, double& top)
{
    // Children of a uipanel sit in its content area, below the title band and
    // inside the border; the widget hosting them covers the whole panel.
    left = 0;
    top = 0;
    std::vector<double> content;
    double frameHeight = 0;
    if (uiParentContentRect(model, content, frameHeight)) {
        left = content[0];
        top = frameHeight - content[1] - content[3];
    }
}
//=============================================================================
void
QtUIComponentBinding::destroy()
{
    if (widget) {
        widget->hide();
        delete widget.data();
        widget.clear();
    }
}
//=============================================================================
bool
QtUIComponentBinding::create()
{
    createWidget();
    return !widget.isNull();
}
//=============================================================================
QWidget*
QtUIComponentBinding::getParentQWidget()
{
    return uiParentQWidget(model, model->getParentFigureHost());
}
//=============================================================================
// Resolve through the container chain: an object parented to a
// Panel/Tab/ButtonGroup/GridLayout is hosted by the container widget.
QWidget*
uiParentQWidget(GraphicsObject* object, GOFigureHost* host)
{
    GOGObjectsProperty* parentProperty = static_cast<GOGObjectsProperty*>(
        object->findProperty(GO_PARENT_PROPERTY_NAME_STR, false));
    if (parentProperty && !parentProperty->data().empty()) {
        int64 parentHandle = parentProperty->data()[0];
        if (parentHandle >= HANDLE_OFFSET_OBJECT) {
            GraphicsObject* parentObject = findGraphicsObject(parentHandle, false);
            GOUIContainer* container = dynamic_cast<GOUIContainer*>(parentObject);
            if (container) {
                UIComponentBinding* containerBinding = container->getBinding();
                if (!containerBinding || !containerBinding->exists()) {
                    container->updateState();
                    containerBinding = container->getBinding();
                }
                if (containerBinding) {
                    QWidget* clientWidget
                        = static_cast<QWidget*>(containerBinding->nativeClientWidget());
                    if (clientWidget) {
                        return clientWidget;
                    }
                }
            }
        }
    }
    if (!host) {
        return nullptr;
    }
    return static_cast<QWidget*>(host->nativeParentWidget());
}
//=============================================================================
void
QtUIComponentBinding::applyCommonProperties(bool isNewWidget)
{
    bool parentChanged = model->hasChanged(GO_PARENT_PROPERTY_NAME_STR);
    if (isNewWidget || parentChanged) {
        applyParentToWidget();
    }
    if (!model->managesOwnGeometry()
        && (isNewWidget || model->hasChanged(GO_POSITION_PROPERTY_NAME_STR)
            || model->hasChanged(GO_INNER_POSITION_PROPERTY_NAME_STR)
            || model->hasChanged(GO_OUTER_POSITION_PROPERTY_NAME_STR))) {
        if (model->hasChanged(GO_OUTER_POSITION_PROPERTY_NAME_STR)
            && !model->hasChanged(GO_POSITION_PROPERTY_NAME_STR)) {
            std::vector<double> outer(
                model->findVectorDoubleProperty(GO_OUTER_POSITION_PROPERTY_NAME_STR));
            if (outer.size() == 4) {
                model->setFourVectorDefault(
                    GO_POSITION_PROPERTY_NAME_STR, outer[0], outer[1], outer[2], outer[3]);
            }
        }
        if (model->hasChanged(GO_INNER_POSITION_PROPERTY_NAME_STR)
            && !model->hasChanged(GO_POSITION_PROPERTY_NAME_STR)) {
            std::vector<double> inner(
                model->findVectorDoubleProperty(GO_INNER_POSITION_PROPERTY_NAME_STR));
            if (inner.size() == 4) {
                model->setFourVectorDefault(
                    GO_POSITION_PROPERTY_NAME_STR, inner[0], inner[1], inner[2], inner[3]);
            }
        }
        applyPositionToWidget();
        model->syncPositionProperties();
    }
    if (isNewWidget || parentChanged || model->hasChanged(GO_VISIBLE_PROPERTY_NAME_STR)) {
        if (model->stringCheck(GO_VISIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR)) {
            widget->show();
        } else {
            widget->hide();
        }
    }
    if (model->haveProperty(GO_ENABLE_PROPERTY_NAME_STR)
        && (isNewWidget || model->hasChanged(GO_ENABLE_PROPERTY_NAME_STR))) {
        widget->setEnabled(
            model->stringCheck(GO_ENABLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR));
    }
    if (model->haveProperty(GO_TOOLTIP_PROPERTY_NAME_STR)
        && (isNewWidget || model->hasChanged(GO_TOOLTIP_PROPERTY_NAME_STR))) {
        widget->setToolTip(
            wstringToQString(model->findStringProperty(GO_TOOLTIP_PROPERTY_NAME_STR)));
    }
    bool fontChanged = model->haveProperty(GO_FONT_NAME_PROPERTY_NAME_STR)
        && (model->hasChanged(GO_FONT_NAME_PROPERTY_NAME_STR)
            || model->hasChanged(GO_FONT_SIZE_PROPERTY_NAME_STR)
            || model->hasChanged(GO_FONT_WEIGHT_PROPERTY_NAME_STR)
            || model->hasChanged(GO_FONT_ANGLE_PROPERTY_NAME_STR));
    if (model->haveProperty(GO_FONT_NAME_PROPERTY_NAME_STR) && (isNewWidget || fontChanged)) {
        applyFontToWidget();
    }
    bool fontColorChanged = model->haveProperty(GO_FONT_COLOR_PROPERTY_NAME_STR)
        && model->hasChanged(GO_FONT_COLOR_PROPERTY_NAME_STR);
    bool backgroundColorChanged = model->haveProperty(GO_BACKGROUND_COLOR_PROPERTY_NAME_STR)
        && model->hasChanged(GO_BACKGROUND_COLOR_PROPERTY_NAME_STR);
    // the style sheet carries the font too: it must be rebuilt on font changes
    if (isNewWidget || fontChanged || fontColorChanged || backgroundColorChanged) {
        applyColorsToWidget();
    }
}
//=============================================================================
void
QtUIComponentBinding::applyParentToWidget()
{
    QWidget* parentWidget = getParentQWidget();
    if (widget && parentWidget && widget->parentWidget() != parentWidget) {
        widget->setParent(parentWidget);
    }
}
//=============================================================================
void
QtUIComponentBinding::applyPositionToWidget()
{
    QWidget* parentWidget = getParentQWidget();
    if (!parentWidget || !widget) {
        return;
    }
    std::vector<double> sizevec(model->findVectorDoubleProperty(GO_POSITION_PROPERTY_NAME_STR));
    if (sizevec.size() != 4) {
        return;
    }
    double parentWidth = 0;
    double parentHeight = 0;
    getParentLayoutSize(parentWidget, parentWidth, parentHeight);
    uiComponentPositionInPixels(model, sizevec, parentWidth, parentHeight);
    double left = 0;
    double top = 0;
    getParentContentOffset(left, top);
    int w = (int)sizevec[2];
    int h = (int)sizevec[3];
    int x = (int)(left + sizevec[0]);
    int y = (int)(top + parentHeight) - (int)sizevec[1] - h;
    QMargins margins = widgetGeometryMargins();
    x -= margins.left();
    y -= margins.top();
    w += margins.left() + margins.right();
    h += margins.top() + margins.bottom();
    widget->setGeometry(x, y, w, h);
}
//=============================================================================
void
QtUIComponentBinding::applyFontToWidget()
{
    if (!widget) {
        return;
    }
    widget->setFont(modelFont());
}
//=============================================================================
QFont
QtUIComponentBinding::modelFont()
{
    QFont font = widget->font();
    std::wstring fontName = model->findStringProperty(GO_FONT_NAME_PROPERTY_NAME_STR);
    font.setFamily(wstringToQString(resolveRootFontName(fontName)));
    double fontSize = model->findScalarDoubleProperty(GO_FONT_SIZE_PROPERTY_NAME_STR);
    if (fontSize > 0) {
        // App Designer components express FontSize in pixels
        font.setPixelSize((int)fontSize);
    }
    font.setBold(model->stringCheck(GO_FONT_WEIGHT_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_BOLD_STR));
    font.setItalic(
        model->stringCheck(GO_FONT_ANGLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ITALIC_STR));
    return font;
}
//=============================================================================
static QString
fontToStyle(const QFont& font)
{
    QString family = font.family();
    family.replace(QLatin1Char('"'), QStringLiteral("\\\""));
    QString style = QString("font-family: \"%1\";").arg(family);
    if (font.pixelSize() > 0) {
        style += QString("font-size: %1px;").arg(font.pixelSize());
    }
    style += font.bold() ? QStringLiteral("font-weight: bold;")
                         : QStringLiteral("font-weight: normal;");
    style += font.italic() ? QStringLiteral("font-style: italic;")
                           : QStringLiteral("font-style: normal;");
    return style;
}
//=============================================================================
static QString
colorToStyle(const QString& name, GOColorProperty* color)
{
    if (!color || color->isNone()) {
        return {};
    }
    std::vector<double> rgb = color->data();
    if (rgb.size() < 3) {
        return {};
    }
    QColor qColor = QColor::fromRgb(
        (int)(rgb[0] * 255.0 + 0.5), (int)(rgb[1] * 255.0 + 0.5), (int)(rgb[2] * 255.0 + 0.5));
    return QString("%1: %2;").arg(name, qColor.name());
}
//=============================================================================
void
QtUIComponentBinding::applyColorsToWidget()
{
    if (!widget) {
        return;
    }
    // selector-less declarations cannot be mixed with selector rules in a Qt
    // style sheet: wrap the common declarations in a universal selector
    QString declarations;
    if (model->haveProperty(GO_BACKGROUND_COLOR_PROPERTY_NAME_STR)) {
        // The property may not be a single color (e.g. uitable stores a per-row
        // color matrix in a GOArrayOfProperty); dynamic_cast yields nullptr for
        // those and the generic stylesheet path is skipped.
        GOColorProperty* backgroundColor = dynamic_cast<GOColorProperty*>(
            model->findProperty(GO_BACKGROUND_COLOR_PROPERTY_NAME_STR, false));
        if (backgroundColor && backgroundColor->isNone()) {
            declarations += QStringLiteral("background: transparent;");
        } else {
            declarations += colorToStyle(QStringLiteral("background-color"), backgroundColor);
        }
    }
    if (model->haveProperty(GO_FONT_COLOR_PROPERTY_NAME_STR)) {
        declarations += colorToStyle(QStringLiteral("color"),
            dynamic_cast<GOColorProperty*>(
                model->findProperty(GO_FONT_COLOR_PROPERTY_NAME_STR, false)));
    }
    // a style sheet rule without font makes Qt replace the widget font by the
    // parent one when polishing: the font set by applyFontToWidget would be lost
    if (model->haveProperty(GO_FONT_NAME_PROPERTY_NAME_STR)) {
        declarations += fontToStyle(modelFont());
    }
    QString styleSheet;
    if (!declarations.isEmpty()) {
        styleSheet = QStringLiteral("* { ") + declarations + QStringLiteral(" }");
    }
    styleSheet += additionalStyleSheet();
    widget->setStyleSheet(styleSheet);
}
//=============================================================================
void
QtUIComponentBinding::show()
{
    if (widget) {
        widget->show();
    }
}
//=============================================================================
void
QtUIComponentBinding::hide()
{
    if (widget) {
        widget->hide();
    }
}
//=============================================================================
void
QtUIComponentBinding::setFocus()
{
    if (widget) {
        widget->raise();
        widget->setFocus();
    }
}
//=============================================================================
bool
QtUIComponentBinding::consumeGeometryChange()
{
    if (!widget) {
        return false;
    }
    // Only a size change counts: a pure move (a pixel Position re-anchored to
    // the bottom of a resized parent) neither fires SizeChangedFcn nor needs
    // the children relaid out, they are held by this widget.
    QSize currentSize = widget->size();
    if (currentSize != lastAppliedSize) {
        lastAppliedSize = currentSize;
        return true;
    }
    return false;
}
//=============================================================================
void
QtUIComponentBinding::renderSnapshot(RenderInterface& gc)
{
    if (!widget) {
        return;
    }
    QImage image(widget->size(), QImage::Format_ARGB32);
    // a translucent widget leaves pixels unpainted: start from transparent,
    // not from uninitialized memory
    image.fill(Qt::transparent);
    QPainter painter(&image);
    widget->render(&painter);
    std::vector<double> sizevec(model->findVectorDoubleProperty(GO_POSITION_PROPERTY_NAME_STR));
    if (sizevec.size() == 4) {
        QWidget* parentWidget = getParentQWidget();
        double width = parentWidget ? parentWidget->width() : gc.width();
        double height = parentWidget ? parentWidget->height() : gc.height();
        getParentLayoutSize(parentWidget, width, height);
        uiComponentPositionInPixels(model, sizevec, width, height);
        int y = gc.height() - (int)sizevec[1] - widget->height();
        QImageRgba rgba(image);
        gc.drawImage((int)sizevec[0], y, rgba.view());
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
