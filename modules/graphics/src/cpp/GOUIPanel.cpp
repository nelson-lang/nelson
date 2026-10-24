//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <algorithm>
#include "GOUIPanel.h"
#include "GOUIControl.h"
#include "GOUIRadioButton.h"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "GOCallbackProperty.hpp"
#include "GOGObjectsProperty.hpp"
#include "GOScalarLogicalProperty.hpp"
#include "GOHelpers.hpp"
#include "GOPositionHelpers.hpp"
#include "GOColorProperty.hpp"
#include "RenderInterface.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static bool
setGroupButtonValue(GraphicsObject* button, bool selected);
static bool
groupButtonIsSelected(GraphicsObject* button);
//=============================================================================
// Chrome metrics: the title sits in a band of its own text height plus this
// padding, inset from the side by the border plus this margin.
static constexpr double TITLE_BAND_PADDING = 4.0;
static constexpr double TITLE_SIDE_PADDING = 4.0;
//=============================================================================
GOUIPanel::GOUIPanel(bool isButtonGroup) : buttonGroupStyle(isButtonGroup)
{
    constructProperties();
    setupDefaults();
}
//=============================================================================
std::wstring
GOUIPanel::getType()
{
    return buttonGroupStyle ? GO_PROPERTY_VALUE_UIBUTTONGROUP_STR : GO_PROPERTY_VALUE_UIPANEL_STR;
}
//=============================================================================
std::string
GOUIPanel::getClassName()
{
    return buttonGroupStyle ? GO_CLASS_NAME_UI_BUTTONGROUP_STR : GO_CLASS_NAME_UI_PANEL_STR;
}
//=============================================================================
void
GOUIPanel::constructProperties()
{
    registerPanelLikeProperties();
    setPropertyWritable(GO_INNER_POSITION_PROPERTY_NAME_STR, false);
    setPropertyWritable(GO_OUTER_POSITION_PROPERTY_NAME_STR, true);
    if (buttonGroupStyle) {
        registerProperty(new GOGObjectsProperty, GO_BUTTONS_PROPERTY_NAME_STR, false);
        registerProperty(new GOGObjectsProperty, GO_SELECTED_OBJECT_PROPERTY_NAME_STR);
        registerProperty(new GOCallbackProperty, GO_SELECTION_CHANGED_FCN_PROPERTY_NAME_STR);
    }
    sortProperties();
}
//=============================================================================
void
GOUIPanel::setupDefaults()
{
    setupPanelLikeDefaults(20, 20, 260, buttonGroupStyle ? 210 : 221);
    if (buttonGroupStyle) {
        setThreeVectorDefault(GO_BORDER_COLOR_PROPERTY_NAME_STR, 1, 1, 1);
        setThreeVectorDefault(GO_HIGHLIGHT_COLOR_PROPERTY_NAME_STR, 1, 1, 1);
    }
}
//=============================================================================
void
GOUIPanel::paintChrome(RenderInterface& gc)
{
    GOUIContainer::paintChrome(gc);
    const double width = gc.width();
    const double height = gc.height();
    const std::wstring title = findStringProperty(GO_TITLE_PROPERTY_NAME_STR);
    const double borderWidth = panelBorderWidth();
    const double band = titleBandHeight(gc);
    gc.setupDirectDraw();
    gc.setLineStyle(L"-");
    if (!title.empty()) {
        auto* foreground = dynamic_cast<GOColorProperty*>(
            findProperty(GO_FOREGROUND_COLOR_PROPERTY_NAME_STR, false));
        std::vector<double> textColor = (foreground != nullptr && !foreground->isNone())
            ? foreground->data()
            : std::vector<double> { 0, 0, 0 };
        const std::wstring titlePosition = findStringProperty(GO_TITLE_POSITION_PROPERTY_NAME_STR);
        double x = borderWidth + TITLE_SIDE_PADDING;
        RenderInterface::AlignmentFlag alignment = RenderInterface::Min;
        if (titlePosition == L"centertop") {
            x = width / 2.0;
            alignment = RenderInterface::Mean;
        } else if (titlePosition == L"righttop") {
            x = width - borderWidth - TITLE_SIDE_PADDING;
            alignment = RenderInterface::Max;
        }
        gc.putText(x, height - borderWidth - band / 2.0, title, textColor, alignment,
            RenderInterface::Mean, chromeFont(), 0);
    }
    if (borderWidth > 0) {
        auto* borderColor = dynamic_cast<GOColorProperty*>(
            findProperty(GO_BORDER_COLOR_PROPERTY_NAME_STR, false));
        if (borderColor != nullptr && !borderColor->isNone()) {
            gc.color(borderColor->data());
            gc.lineWidth(borderWidth);
            const double half = borderWidth / 2.0;
            // The border encloses the whole panel, title band included; a
            // separator closes that band.
            gc.rect(half, half, width - half, height - half);
            if (!title.empty()) {
                const double y = height - borderWidth - band;
                gc.line(0, y, width, y);
            }
        }
    }
    gc.releaseDirectDraw();
}
//=============================================================================
std::vector<double>
GOUIPanel::contentRect(RenderInterface& gc)
{
    const double borderWidth = panelBorderWidth();
    const double band = titleBandHeight(gc);
    const double width = gc.width() - 2 * borderWidth;
    const double height = gc.height() - 2 * borderWidth - band;
    if (width <= 0 || height <= 0) {
        return {};
    }
    return { borderWidth, borderWidth, width, height };
}
//=============================================================================
double
GOUIPanel::panelBorderWidth()
{
    if (!stringCheck(GO_BORDER_TYPE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_LINE_STR)) {
        return 0.0;
    }
    const double borderWidth = findScalarDoubleProperty(GO_BORDER_WIDTH_PROPERTY_NAME_STR);
    return borderWidth > 0 ? borderWidth : 0.0;
}
//=============================================================================
double
GOUIPanel::titleBandHeight(RenderInterface& gc)
{
    if (findStringProperty(GO_TITLE_PROPERTY_NAME_STR).empty()) {
        return 0.0;
    }
    return chromeTextHeight(gc) + TITLE_BAND_PADDING;
}
//=============================================================================
void
GOUIPanel::synchronizePanelPositions()
{
    bool outerChanged = hasChanged(GO_OUTER_POSITION_PROPERTY_NAME_STR);
    if (outerChanged) {
        std::vector<double> outer = findVectorDoubleProperty(GO_OUTER_POSITION_PROPERTY_NAME_STR);
        if (outer.size() == 4) {
            setFourVectorDefault(
                GO_POSITION_PROPERTY_NAME_STR, outer[0], outer[1], outer[2], outer[3]);
            findProperty(GO_POSITION_PROPERTY_NAME_STR)->setModified(true);
        }
        clearChanged(GO_OUTER_POSITION_PROPERTY_NAME_STR);
    } else {
        bool positionChanged = hasChanged(GO_POSITION_PROPERTY_NAME_STR);
        syncSimplePositionTriplet(this);
        if (positionChanged) {
            findProperty(GO_POSITION_PROPERTY_NAME_STR)->setModified(true);
        }
    }
}
//=============================================================================
void
GOUIPanel::deriveInnerPosition()
{
    std::vector<double> position = findVectorDoubleProperty(GO_POSITION_PROPERTY_NAME_STR);
    if (position.size() != 4) {
        return;
    }
    double borderInset = stringCheck(GO_BORDER_TYPE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_LINE_STR)
        ? findScalarDoubleProperty(GO_BORDER_WIDTH_PROPERTY_NAME_STR)
        : 0.0;
    double innerWidth = position[2] - 2 * borderInset;
    double innerHeight = position[3] - 2 * borderInset;
    setFourVectorDefault(GO_INNER_POSITION_PROPERTY_NAME_STR, position[0] + borderInset,
        position[1] + borderInset, innerWidth < 0 ? 0 : innerWidth,
        innerHeight < 0 ? 0 : innerHeight);
    clearChanged(GO_INNER_POSITION_PROPERTY_NAME_STR);
}
//=============================================================================
void
GOUIPanel::syncPositionProperties()
{
    std::vector<double> position = findVectorDoubleProperty(GO_POSITION_PROPERTY_NAME_STR);
    if (position.size() == 4) {
        setFourVectorDefault(GO_OUTER_POSITION_PROPERTY_NAME_STR, position[0], position[1],
            position[2], position[3]);
        clearChanged(GO_OUTER_POSITION_PROPERTY_NAME_STR);
    }
    deriveInnerPosition();
    clearChanged(GO_POSITION_PROPERTY_NAME_STR);
}
//=============================================================================
void
GOUIPanel::updateState()
{
    refreshGroupButtons();
    applySelectedObject();
    synchronizePanelPositions();
    GOUIContainer::updateState();
    if (!binding || !binding->exists()) {
        deriveInnerPosition();
        clearChanged(GO_POSITION_PROPERTY_NAME_STR);
        clearChanged(GO_OUTER_POSITION_PROPERTY_NAME_STR);
    }
}
//=============================================================================
void
GOUIPanel::refreshGroupButtons()
{
    GOGObjectsProperty* buttons
        = static_cast<GOGObjectsProperty*>(findProperty(GO_BUTTONS_PROPERTY_NAME_STR, false));
    if (!buttonGroupStyle || !buttons) {
        return;
    }
    // the read-only Buttons list: radio/toggle children, ui components or uicontrols
    std::vector<int64> handles;
    for (int64 childHandle : getChildrenHandles()) {
        GraphicsObject* child = findGraphicsObject(childHandle, false);
        auto* control = dynamic_cast<GOUIControl*>(child);
        bool controlButton = control
            && (control->stringCheck(GO_STYLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_RADIOBUTTON_STR)
                || control->stringCheck(
                    GO_STYLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_TOGGLEBUTTON_STR));
        if (controlButton || dynamic_cast<GOUIRadioButton*>(child)) {
            handles.push_back(childHandle);
        }
    }
    buttons->data(handles);
}
//=============================================================================
void
GOUIPanel::applySelectedObject()
{
    if (!buttonGroupStyle || !hasChanged(GO_SELECTED_OBJECT_PROPERTY_NAME_STR)) {
        return;
    }
    clearChanged(GO_SELECTED_OBJECT_PROPERTY_NAME_STR);
    GOGObjectsProperty* selectedObject = static_cast<GOGObjectsProperty*>(
        findProperty(GO_SELECTED_OBJECT_PROPERTY_NAME_STR, false));
    GOGObjectsProperty* buttons
        = static_cast<GOGObjectsProperty*>(findProperty(GO_BUTTONS_PROPERTY_NAME_STR, false));
    if (!selectedObject || !buttons) {
        return;
    }
    std::vector<int64> handles(buttons->data());
    int64 target = selectedObject->data().empty() ? -1 : selectedObject->data()[0];
    if (std::find(handles.begin(), handles.end(), target) == handles.end()) {
        // not one of the group buttons: keep the button actually selected
        std::vector<int64> current;
        for (int64 handle : handles) {
            GraphicsObject* button = findGraphicsObject(handle, false);
            if (button && groupButtonIsSelected(button)) {
                current.push_back(handle);
                break;
            }
        }
        selectedObject->data(current);
        return;
    }
    // selected from code: the buttons follow, SelectionChangedFcn does not fire
    for (int64 handle : handles) {
        GraphicsObject* button = findGraphicsObject(handle, false);
        bool selected = handle == target;
        if (button && groupButtonIsSelected(button) != selected
            && setGroupButtonValue(button, selected)) {
            button->updateState();
        }
    }
}
//=============================================================================
void
GOUIPanel::addGroupButton(int64 buttonHandle, bool selected, bool valueGiven)
{
    GOGObjectsProperty* selectedObject = static_cast<GOGObjectsProperty*>(
        findProperty(GO_SELECTED_OBJECT_PROPERTY_NAME_STR, false));
    GraphicsObject* button = findGraphicsObject(buttonHandle, false);
    if (!buttonGroupStyle || !selectedObject || !button) {
        return;
    }
    refreshGroupButtons();
    if (selected) {
        notifyChildSelected(buttonHandle, false);
    } else if (selectedObject->data().empty() && !valueGiven) {
        setGroupButtonValue(button, true);
        notifyChildSelected(buttonHandle, false);
    }
}
//=============================================================================
void
GOUIPanel::notifyChildSelected(int64 childHandle, bool fireEvent)
{
    if (!buttonGroupStyle) {
        return;
    }
    GOGObjectsProperty* selectedObject = static_cast<GOGObjectsProperty*>(
        findProperty(GO_SELECTED_OBJECT_PROPERTY_NAME_STR, false));
    if (!selectedObject) {
        return;
    }
    std::vector<int64> previousSelection(selectedObject->data());
    int64 previousHandle = previousSelection.empty() ? -1 : previousSelection[0];
    if (previousHandle == childHandle) {
        return;
    }
    GraphicsObject* previousButton
        = previousHandle != -1 ? findGraphicsObject(previousHandle, false) : nullptr;
    if (previousButton && setGroupButtonValue(previousButton, false)) {
        previousButton->updateState();
    }
    std::vector<int64> newSelection;
    newSelection.push_back(childHandle);
    selectedObject->data(newSelection);

    if (!fireEvent) {
        return;
    }
    wstringVector fieldNames = { L"OldValue", L"NewValue" };
    ArrayOfVector fieldValues;
    fieldValues << (previousHandle == -1 ? ArrayOf::empty(0, 0)
                                         : graphicsObjectHandle(previousHandle));
    fieldValues << graphicsObjectHandle(childHandle);
    emitCallbackEvent(GO_SELECTION_CHANGED_FCN_PROPERTY_NAME_STR, L"SelectionChangedData",
        L"SelectionChanged", fieldNames, fieldValues);
}
//=============================================================================
// Selects or clears a group button: a ui radio/toggle button holds a logical
// Value, a uicontrol one holds Max when selected and Min otherwise.
static bool
setGroupButtonValue(GraphicsObject* button, bool selected)
{
    GOGenericProperty* value = button->findProperty(GO_VALUE_PROPERTY_NAME_STR, false);
    if (value == nullptr) {
        return false;
    }
    if (auto* logical = dynamic_cast<GOScalarLogicalProperty*>(value)) {
        logical->data(selected);
    } else if (GOGenericProperty* bound = button->findProperty(
                   selected ? GO_MAX_PROPERTY_NAME_STR : GO_MIN_PROPERTY_NAME_STR, false)) {
        value->set(bound->get());
    } else {
        return false;
    }
    value->setModified(true);
    return true;
}
//=============================================================================
static bool
groupButtonIsSelected(GraphicsObject* button)
{
    GOGenericProperty* value = button->findProperty(GO_VALUE_PROPERTY_NAME_STR, false);
    if (auto* logical = dynamic_cast<GOScalarLogicalProperty*>(value)) {
        return logical->data();
    }
    return value != nullptr && button->haveProperty(GO_MAX_PROPERTY_NAME_STR)
        && button->findScalarDoubleProperty(GO_VALUE_PROPERTY_NAME_STR)
        == button->findScalarDoubleProperty(GO_MAX_PROPERTY_NAME_STR);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
