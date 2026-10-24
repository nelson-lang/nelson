//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "UIComponentBuilder.hpp"
#include "GOUIComponent.h"
#include "GOUIContainer.h"
#include "GOUIButton.h"
#include "GOUILabel.h"
#include "GOUIPanel.h"
#include "GOUITabGroup.h"
#include "GOUITab.h"
#include "GOUIGridLayout.h"
#include "GOUICheckBox.h"
#include "GOUIRadioButton.h"
#include "GOUIDropDown.h"
#include "GOUIListBox.h"
#include "GOUIEditField.h"
#include "GOUITextArea.h"
#include "GOUISpinner.h"
#include "GOUISlider.h"
#include "GOUIKnob.h"
#include "GOUIGauge.h"
#include "GOUILamp.h"
#include "GOUISwitch.h"
#include "GOUIDatePicker.h"
#include "GOUIHTML.h"
#include "GOUITable.h"
#include "GOUITree.h"
#include "GOUITreeNode.h"
#include "GOLayoutOptions.hpp"
#include "GOFigure.hpp"
#include "GOFiguresManager.hpp"
#include "GOHelpers.hpp"
#include "GOCallbackProperty.hpp"
#include "GOGObjectsProperty.hpp"
#include "GOScalarDoubleProperty.hpp"
#include "GOScalarLogicalProperty.hpp"
#include "GOVectorProperty.hpp"
#include "GOArrayOfProperty.hpp"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "StringHelpers.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static void
setFigureStringProperty(GOFigure* fig, const std::wstring& name, const std::wstring& value)
{
    GOGenericProperty* property = fig->findProperty(name, false);
    if (property) {
        property->set(ArrayOf::characterArray(value));
    }
}
//=============================================================================
// Creates a figure with uifigure-style defaults (used when an App Designer
// component is created without an explicit parent).
static int64
createNewUIFigure()
{
    int64 figureID = createNewFigure();
    GOFigureHost* window = findGOWindows(figureID);
    if (!window) {
        Error(L"Nelson:graphics:figureCreationFailed", L"Impossible to create figure.");
    }
    GOFigure* fig = window->hostFigure();
    setFigureStringProperty(fig, GO_MENU_BAR_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_NONE_STR);
    setFigureStringProperty(fig, GO_TOOL_BAR_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_NONE_STR);
    setFigureStringProperty(fig, GO_NUMBER_TITLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR);
    setFigureStringProperty(fig, GO_RESIZE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setFigureStringProperty(fig, GO_WINDOW_STYLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_NORMAL_STR);
    setFigureStringProperty(
        fig, GO_AUTO_RESIZE_CHILDREN_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setFigureStringProperty(fig, GO_DOCK_CONTROLS_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR);
    setFigureStringProperty(fig, GO_NEXT_PLOT_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ADD_STR);
    setFigureStringProperty(fig, GO_HANDLE_VISIBILITY_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR);
    setFigureStringProperty(fig, GO_INTEGER_HANDLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR);
    GOGenericProperty* colorProperty = fig->findProperty(GO_COLOR_PROPERTY_NAME_STR, false);
    if (colorProperty) {
        colorProperty->set(
            ArrayOf::doubleRowVector({ 0.96078431372549, 0.96078431372549, 0.96078431372549 }));
    }
    GOGenericProperty* positionProperty = fig->findProperty(GO_POSITION_PROPERTY_NAME_STR, false);
    if (positionProperty) {
        positionProperty->set(ArrayOf::doubleRowVector({ 488, 242, 560, 420 }));
    }
    auto* numberProperty
        = static_cast<GOScalarProperty*>(fig->findProperty(GO_NUMBER_PROPERTY_NAME_STR));
    static_cast<GOVectorProperty*>(numberProperty)->data(std::vector<double> {});
    fig->setExposedPositionOverride(
        { 488, 242, 560, 420 }, { 488, 242, 560, 420 }, { 488.2, 241.8, 560, 420 }, {});
    fig->updateState();
    return figureID;
}
//=============================================================================
static void
wireComponentToParent(
    GraphicsObject* component, int64 componentHandle, const ResolvedUIParent& parent)
{
    GOGObjectsProperty* parentProperty
        = static_cast<GOGObjectsProperty*>(component->findProperty(GO_PARENT_PROPERTY_NAME_STR));
    std::vector<int64> parentHandles;
    parentHandles.push_back(parent.isFigure ? parent.figureID : parent.parentHandle);
    parentProperty->data(parentHandles);

    GraphicsObject* parentObject = nullptr;
    if (parent.isFigure) {
        parentObject = static_cast<GraphicsObject*>(findGOWindows(parent.figureID)->hostFigure());
    } else {
        parentObject = findGraphicsObject(parent.parentHandle);
    }
    GOGObjectsProperty* childrenProperty = static_cast<GOGObjectsProperty*>(
        parentObject->findProperty(GO_CHILDREN_PROPERTY_NAME_STR));
    std::vector<int64> children(childrenProperty->data());
    children.push_back(componentHandle);
    childrenProperty->data(children);
}
//=============================================================================
static void
syncContainerSelectionAfterCreate(
    GOUIComponent* component, int64 componentHandle, const ResolvedUIParent& parent)
{
    if (parent.isFigure) {
        return;
    }
    GraphicsObject* parentObject = findGraphicsObject(parent.parentHandle, false);
    GOUITabGroup* tabGroup = dynamic_cast<GOUITabGroup*>(parentObject);
    if (tabGroup && dynamic_cast<GOUITab*>(component)) {
        GOGObjectsProperty* selectedTab = static_cast<GOGObjectsProperty*>(
            tabGroup->findProperty(GO_SELECTED_TAB_PROPERTY_NAME_STR, false));
        if (selectedTab && selectedTab->data().empty()) {
            selectedTab->data(std::vector<int64> { componentHandle });
        }
        return;
    }
    GOUIPanel* buttonGroup = dynamic_cast<GOUIPanel*>(parentObject);
    GOUIRadioButton* button = dynamic_cast<GOUIRadioButton*>(component);
    if (!buttonGroup || !buttonGroup->isButtonGroup() || !button) {
        return;
    }
    GOScalarLogicalProperty* value = static_cast<GOScalarLogicalProperty*>(
        button->findProperty(GO_VALUE_PROPERTY_NAME_STR, false));
    if (!value) {
        return;
    }
    buttonGroup->addGroupButton(
        componentHandle, value->data(), button->hasChanged(GO_VALUE_PROPERTY_NAME_STR));
}
//=============================================================================
static bool
parentIsGridLayout(const ResolvedUIParent& parent)
{
    if (parent.isFigure) {
        return false;
    }
    GraphicsObject* object = findGraphicsObject(parent.parentHandle, false);
    return dynamic_cast<GOUIGridLayout*>(object) != nullptr;
}
//=============================================================================
static void
attachGridLayoutOptions(GraphicsObject* component, int64 componentHandle)
{
    GOLayoutOptions* options = new GOLayoutOptions(LayoutOptionsMode::GridLayout);
    int64 optionsHandle = assignGraphicsObject(options);
    options->setGoProperty(GO_PARENT_PROPERTY_NAME_STR, componentHandle);
    GOGObjectsProperty* layoutProperty = static_cast<GOGObjectsProperty*>(
        component->findProperty(GO_LAYOUT_PROPERTY_NAME_STR, false));
    if (layoutProperty) {
        std::vector<int64> handles;
        handles.push_back(optionsHandle);
        layoutProperty->data(handles);
    }
}
//=============================================================================
static ArrayOf
weightedTrackCell(size_t count)
{
    ArrayOf* elements = (ArrayOf*)ArrayOf::allocateArrayOf(NLS_CELL_ARRAY, count);
    for (size_t k = 0; k < count; ++k) {
        elements[k] = ArrayOf::characterArray(L"1x");
    }
    return ArrayOf(NLS_CELL_ARRAY, Dimensions(1, count), elements);
}
//=============================================================================
go_handle
UIComponentBuilder(const ArrayOfVector& argIn, const UIComponentBuilderOptions& options,
    const UIComponentFactory& factory)
{
    ArrayOfVector args(argIn);
    ResolvedUIParent parent;
    bool haveParent = false;

    if (!args.empty() && args[0].isGraphicsObject()) {
        parent = resolveUIParent(args[0]);
        haveParent = true;
        args.pop_front();
    }

    std::wstring style = options.defaultStyle;
    if (!args.empty() && !options.allowedStyles.empty()
        && (args[0].isRowVectorCharacterArray() || args[0].isScalarStringArray())) {
        std::wstring candidate = args[0].asWideString();
        StringHelpers::to_lower(candidate);
        for (const std::wstring& allowedStyle : options.allowedStyles) {
            if (candidate == allowedStyle) {
                style = candidate;
                args.pop_front();
                break;
            }
        }
    }

    // uigridlayout(parent, [rows cols])
    size_t sizeRows = 0;
    size_t sizeColumns = 0;
    if (options.acceptsSizeArgument && !args.empty() && args[0].isNumeric()
        && args[0].elementCount() == 2) {
        ArrayOf sizeValue(args[0]);
        sizeValue.promoteType(NLS_DOUBLE);
        const double* values = (const double*)sizeValue.dataPointer();
        if (values[0] >= 1 && values[1] >= 1) {
            sizeRows = (size_t)values[0];
            sizeColumns = (size_t)values[1];
            args.pop_front();
        }
    }

    if (args.size() % 2 != 0) {
        Error(L"Nelson:uicomponent:pvPairsExpected",
            L"Wrong number of input arguments: property/value pairs expected.");
    }
    for (size_t k = 0; k + 1 < args.size(); k += 2) {
        if (!(args[k].isRowVectorCharacterArray() || args[k].isScalarStringArray())) {
            Error(L"Nelson:uicomponent:invalidPropertyName",
                L"Property name expected: character vector or scalar string.");
        }
        std::wstring propertyName = args[k].asWideString();
        if (propertyName == GO_PARENT_PROPERTY_NAME_STR) {
            parent = resolveUIParent(args[k + 1]);
            haveParent = true;
        }
    }

    if (!haveParent) {
        int64 figureID = createNewUIFigure();
        parent.figureID = figureID;
        parent.parentHandle = figureID;
        parent.isFigure = true;
    }

    // implicit tab group creation (uitab(fig) or uitab(panel))
    if (options.parentKind == UIParentKind::TabGroupOnly && !parentIsTabGroup(parent)) {
        GOUITabGroup* tabGroup = new GOUITabGroup;
        int64 tabGroupHandle = assignGraphicsObject(tabGroup);
        tabGroup->buildWidget(findGOWindows(parent.figureID));
        wireComponentToParent(tabGroup, tabGroupHandle, parent);
        if (parentIsGridLayout(parent)) {
            attachGridLayoutOptions(tabGroup, tabGroupHandle);
        }
        tabGroup->updateState();
        parent.parentHandle = tabGroupHandle;
        parent.isFigure = false;
    }
    checkParentKind(options.parentKind, parent);

    GOUIComponent* go = factory(style);
    go_handle thisHandle = assignGraphicsObject(go);
    go->buildWidget(findGOWindows(parent.figureID));
    wireComponentToParent(go, thisHandle, parent);
    if (parentIsGridLayout(parent)) {
        attachGridLayoutOptions(go, thisHandle);
    }

    if (sizeRows > 0 && sizeColumns > 0) {
        GOArrayOfProperty* rowHeight = static_cast<GOArrayOfProperty*>(
            go->findProperty(GO_ROW_HEIGHT_PROPERTY_NAME_STR, false));
        GOArrayOfProperty* columnWidth = static_cast<GOArrayOfProperty*>(
            go->findProperty(GO_COLUMN_WIDTH_PROPERTY_NAME_STR, false));
        if (rowHeight && columnWidth) {
            rowHeight->data(weightedTrackCell(sizeRows));
            columnWidth->data(weightedTrackCell(sizeColumns));
        }
    }

    while (args.size() >= 2) {
        std::wstring propertyName(args[0].asWideString());
        ArrayOf propertyValue(args[1]);
        if (propertyName != GO_PARENT_PROPERTY_NAME_STR) {
            try {
                GOGenericProperty* goProperty = go->findProperty(propertyName);
                if (!go->isWritable(propertyName)) {
                    Error(L"Nelson:uicomponent:readOnlyProperty", L"Property is read-only: %s",
                        propertyName);
                }
                goProperty->set(propertyValue);
            } catch (const NelsonException& e) {
                RaiseError(formatI18nMessage(
                               Nelson::resolveMessageTemplate(L"Nelson:graphics:gotErrorProperty",
                                   L"Got error for property: %s"),
                               propertyName)
                        + L"\n" + e.what(),
                    L"Nelson:uicomponent:propertyError");
            }
        }
        args.pop_front();
        args.pop_front();
    }
    syncContainerSelectionAfterCreate(go, thisHandle, parent);
    go->updateState();

    // refresh the parent container (grid layout placement, Buttons list, ...)
    if (!parent.isFigure) {
        GraphicsObject* parentObject = findGraphicsObject(parent.parentHandle, false);
        if (parentObject) {
            parentObject->updateState();
        }
        GOUIGridLayout* grid = dynamic_cast<GOUIGridLayout*>(parentObject);
        if (grid) {
            grid->performLayout();
        }
    }

    GOCallbackProperty* goCallback
        = (GOCallbackProperty*)go->findProperty(GO_CREATE_FCN_PROPERTY_NAME_STR);
    if (goCallback) {
        goCallback->executeNow(go);
    }
    return thisHandle;
}
//=============================================================================
go_handle
UIButtonBuilder(const ArrayOfVector& argIn)
{
    UIComponentBuilderOptions options;
    options.allowedStyles = { L"push", L"state" };
    options.defaultStyle = L"push";
    return UIComponentBuilder(argIn, options, [](const std::wstring& style) -> GOUIComponent* {
        return new GOUIButton(style == L"state");
    });
}
//=============================================================================
go_handle
UILabelBuilder(const ArrayOfVector& argIn)
{
    UIComponentBuilderOptions options;
    return UIComponentBuilder(
        argIn, options, [](const std::wstring&) -> GOUIComponent* { return new GOUILabel; });
}
//=============================================================================
go_handle
UIPanelBuilder(const ArrayOfVector& argIn)
{
    UIComponentBuilderOptions options;
    return UIComponentBuilder(
        argIn, options, [](const std::wstring&) -> GOUIComponent* { return new GOUIPanel(false); });
}
//=============================================================================
go_handle
UIButtonGroupBuilder(const ArrayOfVector& argIn)
{
    UIComponentBuilderOptions options;
    return UIComponentBuilder(
        argIn, options, [](const std::wstring&) -> GOUIComponent* { return new GOUIPanel(true); });
}
//=============================================================================
go_handle
UITabGroupBuilder(const ArrayOfVector& argIn)
{
    UIComponentBuilderOptions options;
    return UIComponentBuilder(
        argIn, options, [](const std::wstring&) -> GOUIComponent* { return new GOUITabGroup; });
}
//=============================================================================
go_handle
UITabBuilder(const ArrayOfVector& argIn)
{
    UIComponentBuilderOptions options;
    options.parentKind = UIParentKind::TabGroupOnly;
    return UIComponentBuilder(
        argIn, options, [](const std::wstring&) -> GOUIComponent* { return new GOUITab; });
}
//=============================================================================
go_handle
UIGridLayoutBuilder(const ArrayOfVector& argIn)
{
    UIComponentBuilderOptions options;
    options.acceptsSizeArgument = true;
    return UIComponentBuilder(
        argIn, options, [](const std::wstring&) -> GOUIComponent* { return new GOUIGridLayout; });
}
//=============================================================================
go_handle
UICheckBoxBuilder(const ArrayOfVector& argIn)
{
    UIComponentBuilderOptions options;
    return UIComponentBuilder(
        argIn, options, [](const std::wstring&) -> GOUIComponent* { return new GOUICheckBox; });
}
//=============================================================================
go_handle
UIRadioButtonBuilder(const ArrayOfVector& argIn)
{
    UIComponentBuilderOptions options;
    options.parentKind = UIParentKind::ButtonGroupOnly;
    return UIComponentBuilder(argIn, options,
        [](const std::wstring&) -> GOUIComponent* { return new GOUIRadioButton(false); });
}
//=============================================================================
go_handle
UIToggleButtonBuilder(const ArrayOfVector& argIn)
{
    UIComponentBuilderOptions options;
    options.parentKind = UIParentKind::ButtonGroupOnly;
    return UIComponentBuilder(argIn, options,
        [](const std::wstring&) -> GOUIComponent* { return new GOUIRadioButton(true); });
}
//=============================================================================
go_handle
UIDropDownBuilder(const ArrayOfVector& argIn)
{
    UIComponentBuilderOptions options;
    return UIComponentBuilder(
        argIn, options, [](const std::wstring&) -> GOUIComponent* { return new GOUIDropDown; });
}
//=============================================================================
go_handle
UIListBoxBuilder(const ArrayOfVector& argIn)
{
    UIComponentBuilderOptions options;
    return UIComponentBuilder(
        argIn, options, [](const std::wstring&) -> GOUIComponent* { return new GOUIListBox; });
}
//=============================================================================
go_handle
UIEditFieldBuilder(const ArrayOfVector& argIn)
{
    UIComponentBuilderOptions options;
    options.allowedStyles = { L"text", L"numeric" };
    options.defaultStyle = L"text";
    return UIComponentBuilder(argIn, options, [](const std::wstring& style) -> GOUIComponent* {
        return new GOUIEditField(style == L"numeric");
    });
}
//=============================================================================
go_handle
UITextAreaBuilder(const ArrayOfVector& argIn)
{
    UIComponentBuilderOptions options;
    return UIComponentBuilder(
        argIn, options, [](const std::wstring&) -> GOUIComponent* { return new GOUITextArea; });
}
//=============================================================================
go_handle
UISpinnerBuilder(const ArrayOfVector& argIn)
{
    UIComponentBuilderOptions options;
    return UIComponentBuilder(
        argIn, options, [](const std::wstring&) -> GOUIComponent* { return new GOUISpinner; });
}
//=============================================================================
go_handle
UISliderBuilder(const ArrayOfVector& argIn)
{
    UIComponentBuilderOptions options;
    options.allowedStyles = { L"slider", L"range" };
    options.defaultStyle = L"slider";
    return UIComponentBuilder(argIn, options, [](const std::wstring& style) -> GOUIComponent* {
        return new GOUISlider(style == L"range");
    });
}
//=============================================================================
go_handle
UIKnobBuilder(const ArrayOfVector& argIn)
{
    UIComponentBuilderOptions options;
    options.allowedStyles = { L"continuous", L"discrete" };
    options.defaultStyle = L"continuous";
    return UIComponentBuilder(argIn, options, [](const std::wstring& style) -> GOUIComponent* {
        return new GOUIKnob(style == L"discrete");
    });
}
//=============================================================================
go_handle
UIGaugeBuilder(const ArrayOfVector& argIn)
{
    UIComponentBuilderOptions options;
    options.allowedStyles = { L"circular", L"linear", L"ninetydegree", L"semicircular" };
    options.defaultStyle = L"circular";
    return UIComponentBuilder(argIn, options, [](const std::wstring& style) -> GOUIComponent* {
        UIGaugeStyle gaugeStyle = UIGaugeStyle::Circular;
        if (style == L"linear") {
            gaugeStyle = UIGaugeStyle::Linear;
        } else if (style == L"ninetydegree") {
            gaugeStyle = UIGaugeStyle::NinetyDegree;
        } else if (style == L"semicircular") {
            gaugeStyle = UIGaugeStyle::Semicircular;
        }
        return new GOUIGauge(gaugeStyle);
    });
}
//=============================================================================
go_handle
UILampBuilder(const ArrayOfVector& argIn)
{
    UIComponentBuilderOptions options;
    return UIComponentBuilder(
        argIn, options, [](const std::wstring&) -> GOUIComponent* { return new GOUILamp; });
}
//=============================================================================
go_handle
UISwitchBuilder(const ArrayOfVector& argIn)
{
    UIComponentBuilderOptions options;
    options.allowedStyles = { L"slider", L"rocker", L"toggle" };
    options.defaultStyle = L"slider";
    return UIComponentBuilder(argIn, options, [](const std::wstring& style) -> GOUIComponent* {
        UISwitchStyle switchStyle = UISwitchStyle::Slider;
        if (style == L"rocker") {
            switchStyle = UISwitchStyle::Rocker;
        } else if (style == L"toggle") {
            switchStyle = UISwitchStyle::Toggle;
        }
        return new GOUISwitch(switchStyle);
    });
}
//=============================================================================
go_handle
UIDatePickerBuilder(const ArrayOfVector& argIn)
{
    UIComponentBuilderOptions options;
    return UIComponentBuilder(
        argIn, options, [](const std::wstring&) -> GOUIComponent* { return new GOUIDatePicker; });
}
//=============================================================================
go_handle
UITableBuilder(const ArrayOfVector& argIn)
{
    UIComponentBuilderOptions options;
    return UIComponentBuilder(
        argIn, options, [](const std::wstring&) -> GOUIComponent* { return new GOUITable; });
}
//=============================================================================
go_handle
UIHtmlBuilder(const ArrayOfVector& argIn)
{
    UIComponentBuilderOptions options;
    return UIComponentBuilder(
        argIn, options, [](const std::wstring&) -> GOUIComponent* { return new GOUIHTML; });
}
//=============================================================================
go_handle
UITreeBuilder(const ArrayOfVector& argIn)
{
    UIComponentBuilderOptions options;
    options.allowedStyles = { L"tree", L"checkbox" };
    options.defaultStyle = L"tree";
    return UIComponentBuilder(argIn, options, [](const std::wstring& style) -> GOUIComponent* {
        return new GOUITree(style == L"checkbox");
    });
}
//=============================================================================
go_handle
UITreeNodeBuilder(const ArrayOfVector& argIn)
{
    ArrayOfVector args(argIn);
    int64 parentHandle = -1;
    GraphicsObject* parentObject = nullptr;

    if (!args.empty() && args[0].isGraphicsObject()) {
        go_handle go = args[0].asGraphicsObjectScalar();
        parentObject = findGraphicsObject(go, false);
        if (!dynamic_cast<GOUITree*>(parentObject) && !dynamic_cast<GOUITreeNode*>(parentObject)) {
            Error(L"Nelson:uicomponent:parentMustBeTree",
                L"Parent must be a Tree or TreeNode object.");
        }
        parentHandle = go;
        args.pop_front();
    } else {
        // no parent: create a tree in a new uifigure
        parentHandle = UITreeBuilder(ArrayOfVector());
        parentObject = findGraphicsObject(parentHandle, false);
    }
    if (args.size() % 2 != 0) {
        Error(L"Nelson:uicomponent:pvPairsExpected",
            L"Wrong number of input arguments: property/value pairs expected.");
    }

    GOUITreeNode* node = new GOUITreeNode;
    go_handle nodeHandle = assignGraphicsObject(node);
    GOGObjectsProperty* parentProperty
        = static_cast<GOGObjectsProperty*>(node->findProperty(GO_PARENT_PROPERTY_NAME_STR));
    std::vector<int64> parentHandles;
    parentHandles.push_back(parentHandle);
    parentProperty->data(parentHandles);
    GOGObjectsProperty* childrenProperty = static_cast<GOGObjectsProperty*>(
        parentObject->findProperty(GO_CHILDREN_PROPERTY_NAME_STR));
    std::vector<int64> children(childrenProperty->data());
    children.push_back(nodeHandle);
    childrenProperty->data(children);

    while (args.size() >= 2) {
        std::wstring propertyName(args[0].asWideString());
        ArrayOf propertyValue(args[1]);
        try {
            GOGenericProperty* goProperty = node->findProperty(propertyName);
            if (!node->isWritable(propertyName)) {
                Error(L"Nelson:uicomponent:readOnlyProperty", L"Property is read-only: %s",
                    propertyName);
            }
            goProperty->set(propertyValue);
        } catch (const NelsonException& e) {
            RaiseError(formatI18nMessage(
                           Nelson::resolveMessageTemplate(
                               L"Nelson:graphics:gotErrorProperty", L"Got error for property: %s"),
                           propertyName)
                    + L"\n" + e.what(),
                L"Nelson:uicomponent:propertyError");
        }
        args.pop_front();
        args.pop_front();
    }
    node->updateState();

    GOCallbackProperty* goCallback
        = (GOCallbackProperty*)node->findProperty(GO_CREATE_FCN_PROPERTY_NAME_STR);
    if (goCallback) {
        goCallback->executeNow(node);
    }
    return nodeHandle;
}
//=============================================================================
void
UITreeExpand(go_handle handle, bool expand)
{
    GraphicsObject* object = findGraphicsObject(handle, false);
    GOUITree* tree = dynamic_cast<GOUITree*>(object);
    if (!tree) {
        GOUITreeNode* node = dynamic_cast<GOUITreeNode*>(object);
        if (node) {
            tree = node->getOwningTree();
        }
    }
    if (!tree) {
        Error(L"Nelson:uicomponent:expectedTree", L"Tree or TreeNode graphics object expected.");
    }
    tree->expandAll(expand);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
