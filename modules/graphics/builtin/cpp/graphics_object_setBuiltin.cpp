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
#include <cmath>
#include <utility>
#include "graphics_object_setBuiltin.hpp"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "GraphicsObject.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "NelsonException.hpp"
#include "GOGObjectsProperty.hpp"
#include "GOAnnotation.hpp"
#include "GOAxis.hpp"
#include "GOFigure.hpp"
#include "GOHelpers.hpp"
#include "GORoot.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "GOFiguresManager.hpp"
#include "GOTiledChartLayout.hpp"
#include "GOColorBar.hpp"
#include "GOLegend.hpp"
#include "GOMenuBuilder.hpp"
#include "GOScalarLogicalProperty.hpp"
#include "GOStringProperty.hpp"
#include "GOVectorProperty.hpp"
#include "StringHelpers.hpp"
#include "UIParentRules.hpp"
#include "Evaluator.hpp"
#include "FunctionDef.hpp"
#include "NelsonConfiguration.hpp"
//=============================================================================
namespace Nelson::GraphicsGateway {
//=============================================================================
static void
setChildren(GraphicsObject* fp, ArrayOf childrenToAdd);
//=============================================================================
static void
addChildReference(go_handle parentHandle, go_handle childHandle);
//=============================================================================
static void
removeChildReference(go_handle parentHandle, go_handle childHandle);
//=============================================================================
static void
setParent(GraphicsObject* fp, ArrayOf newParent);
//=============================================================================
static bool
setTiledLayoutProperty(GraphicsObject* fp, const std::wstring& propname, ArrayOf value);
//=============================================================================
static void
updateParentTiledLayoutIfNeeded(GraphicsObject* fp);
//=============================================================================
static void
setObjectReferenceProperty(GraphicsObject* fp, const std::wstring& propname, ArrayOf value);
//=============================================================================
static void
setContextMenuProperty(GraphicsObject* fp, ArrayOf value);
//=============================================================================
static void
checkPropertyAppliesToObject(GraphicsObject* fp, const std::wstring& propname);
//=============================================================================
static void
checkVisiblePropertyAppliesToObject(GraphicsObject* fp, const std::wstring& propname);
//=============================================================================
static std::wstring
canonicalObjectPropertyName(GraphicsObject* fp, const std::wstring& requested);
//=============================================================================
static std::wstring
themeNameFromValue(const ArrayOf& value);
//=============================================================================
static bool
isReplaceableTextReferenceProperty(const std::wstring& propname);
//=============================================================================
static bool
isTiledLayoutSubplotTextFontSizeMode(GraphicsObject* fp, const std::wstring& propname);
//=============================================================================
static void
deleteReplacedTextObjects(
    const std::vector<int64>& oldHandles, const std::vector<int64>& newHandles);
//=============================================================================
static bool
propertyValueAlreadySet(GOGenericProperty* hp, const ArrayOf& value);
//=============================================================================
static bool
isGridModeSetAfterValue(const std::wstring& propname, const ArrayOf& value,
    const std::vector<std::wstring>& namesSetInCall);
//=============================================================================
static bool
stringPropertyAlreadySet(GOGenericProperty* hp, const ArrayOf& value);
//=============================================================================
static bool
vectorPropertyAlreadySet(GOGenericProperty* hp, const ArrayOf& value);
//=============================================================================
static bool
logicalPropertyAlreadySet(GOGenericProperty* hp, const ArrayOf& value);
//=============================================================================
static bool
gobjectsPropertyAlreadySet(GOGenericProperty* hp, const ArrayOf& value);
//=============================================================================
static ArrayOfVector
graphics_object_set_introspection(const ArrayOfVector& argIn);
//=============================================================================
static GraphicsObject*
resolveGraphicsObject(nelson_handle handle);
//=============================================================================
static bool
isAutoLimitModeRequest(GraphicsObject* fp, const std::wstring& propname, const ArrayOf& value);
//=============================================================================
static bool
canMoveAutoLimits(const std::wstring& propname);
//=============================================================================
static bool
hasAutoLimits(GOAxis* axis);
//=============================================================================
static bool
isLimitsContributor(GOAxis* axis, GraphicsObject* object);
//=============================================================================
static bool
isPolarGridProperty(const std::wstring& propname);
//=============================================================================
static void
refreshPolarGrid(GraphicsObject* axisObject);
//=============================================================================
static bool
isHeatmapChartProperty(const std::wstring& propname);
//=============================================================================
static void
refreshHeatmapChart(GraphicsObject* chartObject);
//=============================================================================
static bool
isWordCloudChartProperty(const std::wstring& propname);
//=============================================================================
static void
refreshWordCloudChart(GraphicsObject* chartObject);
//=============================================================================
static ArrayOf
choicesCellFromProperty(GraphicsObject* fp, const std::wstring& propname);
//=============================================================================
ArrayOfVector
graphics_object_setBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1);

    if (!argIn[0].isGraphicsObject()) {
        Error(L"Nelson:graphics:expectedGraphicsObject:expectedGraphicsObjectS",
            L"Expected graphics object(s).");
    }
    // One- and two-argument introspection forms: set(h) returns a struct whose
    // fields are the settable properties (values are the cell arrays of allowed
    // enumerated choices, empty for free-form ones); set(h, 'Prop') returns the
    // cell array of allowed choices for that property.
    const bool isQueryForm = argIn.size() == 2
        && (argIn[1].isRowVectorCharacterArray() || argIn[1].isScalarStringArray());
    if (argIn.size() == 1 || isQueryForm) {
        return graphics_object_set_introspection(argIn);
    }
    nelson_handle* gobjects = (nelson_handle*)(argIn[0].dataPointer());
    indexType nbElements = argIn[0].elementCount();
    for (indexType k = 0; k < nbElements; k++) {
        GraphicsObject* fp = nullptr;
        if (gobjects[k] == HANDLE_ROOT_OBJECT) {
            fp = getGraphicsRootObject();
        } else if (gobjects[k] >= HANDLE_OFFSET_OBJECT) {
            fp = findGraphicsObject(gobjects[k]);
        } else {
            fp = static_cast<GraphicsObject*>(findGOFigure(gobjects[k]));
        }
        if (!fp) {
            Error(L"Nelson:graphics:invalidHandle", L"Invalid handle.");
        }
        // UserData and the color/line-style cycling indexes never affect the object state.
        const std::wstring singlePropertyName = argIn.size() == 3
            ? canonicalObjectPropertyName(fp, argIn[1].asWideString())
            : std::wstring();
        const bool userDataOnlyUpdate = singlePropertyName == GO_USER_DATA_PROPERTY_NAME_STR
            || singlePropertyName == GO_COLOR_ORDER_INDEX_PROPERTY_NAME_STR
            || singlePropertyName == GO_LINE_STYLE_ORDER_INDEX_PROPERTY_NAME_STR;
        int ptr = 1;
        bool objectChanged = false;
        std::vector<std::pair<GOGenericProperty*, ArrayOf>> savedPropertyValues;
        // A child edit can move the automatic limits of its axes, like in MATLAB.
        GOAxis* limitsOwner = fp->getParentAxis();
        bool limitsAffected = false;
        bool autoLimitsRequested = false;
        bool polarGridRefreshNeeded = false;
        bool heatmapRefreshNeeded = false;
        bool wordcloudRefreshNeeded = false;
        std::vector<std::wstring> namesSetInCall;
        std::vector<std::wstring> gridModesForcedAuto;
        while (argIn.size() >= (ptr + 2)) {
            std::wstring propname = argIn[ptr].asWideString();
            if (fp->isType(GO_PROPERTY_VALUE_ROOT_STR)
                && static_cast<GORoot*>(fp)->isDefaultPropertyName(propname)) {
                static_cast<GORoot*>(fp)->setDefaultProperty(propname, argIn[ptr + 1]);
                objectChanged = true;
            } else {
                propname = canonicalObjectPropertyName(fp, propname);
                if (fp->isType(GO_PROPERTY_VALUE_ROOT_STR)
                    && propname == GO_CURRENT_FIGURE_PROPERTY_NAME_STR) {
                    static_cast<GORoot*>(fp)->setCurrentFigureProperty(argIn[ptr + 1]);
                    objectChanged = true;
                } else if (fp->isType(GO_PROPERTY_VALUE_ROOT_STR)
                    && propname == GO_PARENT_PROPERTY_NAME_STR) {
                    if (!argIn[ptr + 1].isEmpty()) {
                        Error(L"Nelson:set:invalidRootParent", L"Invalid root parent.");
                    }
                } else if (propname == GO_CHILDREN_PROPERTY_NAME_STR) {
                    setChildren(fp, argIn[ptr + 1]);
                    objectChanged = true;
                } else if (propname == GO_PARENT_PROPERTY_NAME_STR) {
                    setParent(fp, argIn[ptr + 1]);
                    objectChanged = true;
                } else if (fp->getType() == GO_PROPERTY_VALUE_TILED_LAYOUT_STR) {
                    objectChanged
                        = setTiledLayoutProperty(fp, propname, argIn[ptr + 1]) || objectChanged;
                } else {
                    try {
                        checkPropertyAppliesToObject(fp, propname);
                        checkVisiblePropertyAppliesToObject(fp, propname);
                        GOGenericProperty* hp = fp->findProperty(propname);
                        if (hp) {
                            if (fp->isType(GO_PROPERTY_VALUE_FIGURE_STR)
                                && propname == GO_THEME_PROPERTY_NAME_STR) {
                                static_cast<GOFigure*>(fp)->setTheme(
                                    themeNameFromValue(argIn[ptr + 1]), true);
                            } else if (fp->isType(GO_PROPERTY_VALUE_FIGURE_STR)
                                && propname == GO_THEME_MODE_PROPERTY_NAME_STR) {
                                static_cast<GOFigure*>(fp)->setThemeMode(
                                    argIn[ptr + 1].asWideString());
                            } else if (!fp->isWritable(propname) && fp->haveProperty(propname)) {
                                Error(L"Nelson:graphics:readOnlyProperty",
                                    L"Property is read-only: %s", propname);
                            }
                            if (propname == GO_CONTEXT_MENU_PROPERTY_NAME_STR) {
                                setContextMenuProperty(fp, argIn[ptr + 1]);
                                objectChanged = true;
                            } else if (isReplaceableTextReferenceProperty(propname)
                                && dynamic_cast<GOGObjectsProperty*>(hp)) {
                                setObjectReferenceProperty(fp, propname, argIn[ptr + 1]);
                                objectChanged = true;
                            } else if (!(fp->isType(GO_PROPERTY_VALUE_FIGURE_STR)
                                           && (propname == GO_THEME_PROPERTY_NAME_STR
                                               || propname == GO_THEME_MODE_PROPERTY_NAME_STR))) {
                                // An explicit set must still flip a companion
                                // <Name>Mode property from auto to manual, even
                                // when the stored value does not change.
                                if (isGridModeSetAfterValue(
                                        propname, argIn[ptr + 1], namesSetInCall)) {
                                    gridModesForcedAuto.push_back(propname);
                                }
                                namesSetInCall.push_back(propname);
                                std::wstring modePropertyName = propname + L"Mode";
                                bool flipsAutoMode = fp->haveProperty(modePropertyName)
                                    && fp->isAuto(modePropertyName);
                                const bool valueAlreadySet
                                    = propertyValueAlreadySet(hp, argIn[ptr + 1]);
                                const bool refreshesTitleLayout
                                    = propname == GO_TITLE_HORIZONTAL_ALIGNMENT_PROPERTY_NAME_STR;
                                if (valueAlreadySet
                                    && isAutoLimitModeRequest(fp, propname, argIn[ptr + 1])) {
                                    autoLimitsRequested = true;
                                }
                                if (!valueAlreadySet || flipsAutoMode || refreshesTitleLayout) {
                                    savedPropertyValues.emplace_back(hp, hp->get());
                                    hp->set(argIn[ptr + 1]);
                                    objectChanged = true;
                                    limitsAffected = limitsAffected
                                        || (limitsOwner && canMoveAutoLimits(propname));
                                    if (isPolarGridProperty(propname)) {
                                        polarGridRefreshNeeded = true;
                                    }
                                    if (fp->isType(GO_PROPERTY_VALUE_HEATMAP_STR)
                                        && isHeatmapChartProperty(propname)) {
                                        heatmapRefreshNeeded = true;
                                    }
                                    if (fp->isType(GO_PROPERTY_VALUE_WORD_CLOUD_STR)
                                        && isWordCloudChartProperty(propname)) {
                                        wordcloudRefreshNeeded = true;
                                    }
                                }
                                if (objectChanged && dynamic_cast<GOAnnotation*>(fp)) {
                                    fp->updateState();
                                }
                            }
                        }
                    } catch (const NelsonException& e) {
                        for (auto it = savedPropertyValues.rbegin();
                            it != savedPropertyValues.rend(); ++it) {
                            try {
                                it->first->set(it->second);
                            } catch (...) {
                            }
                        }
                        savedPropertyValues.clear();
                        RaiseError(formatI18nMessage(Nelson::resolveMessageTemplate(
                                                         L"Nelson:graphics:gotErrorProperty",
                                                         L"Got error for property: %s"),
                                       propname)
                                + L"\n" + e.what(),
                            L"Nelson:graphics:propertyError");
                    }
                }
            }
            ptr += 2;
        }
        if (objectChanged && userDataOnlyUpdate) {
            fp->clearAllChanged();
        } else if (objectChanged) {
            try {
                fp->updateState();
            } catch (...) {
                for (auto it = savedPropertyValues.rbegin(); it != savedPropertyValues.rend();
                    ++it) {
                    try {
                        it->first->set(it->second);
                    } catch (...) {
                    }
                }
                // set() flags the restored property as modified. A successful
                // updateState() always ends by clearing the changed flags, so a
                // failed one must too: otherwise a leftover "changed" flag steers
                // the next updateState() down a stale branch (e.g. a dropdown then
                // rejects a valid Items change instead of resetting its selection).
                fp->clearAllChanged();
                throw;
            }
            savedPropertyValues.clear();
            // A grid <Name>Mode given as 'auto' after its value wins over the value.
            if (!gridModesForcedAuto.empty()) {
                for (const std::wstring& modeName : gridModesForcedAuto) {
                    fp->findProperty(modeName)->set(
                        ArrayOf::characterArray(GO_PROPERTY_VALUE_AUTO_STR));
                }
                fp->updateState();
            }
            if (limitsAffected && hasAutoLimits(limitsOwner)
                && isLimitsContributor(limitsOwner, fp)) {
                limitsOwner->markStateDirty();
            }
            // Rebuild the polar grid before the figure repaints, so the grid and
            // the plotted data share the new orientation and styling.
            if (polarGridRefreshNeeded) {
                refreshPolarGrid(fp);
            }
            // Rebuild the heatmap's internal primitives so a changed property
            // (colour limits, labels, grid...) takes effect like in MATLAB.
            if (heatmapRefreshNeeded) {
                refreshHeatmapChart(fp);
            }
            // Re-lay out the word cloud so a changed property (colours, shape,
            // word count...) takes effect like in MATLAB.
            if (wordcloudRefreshNeeded) {
                refreshWordCloudChart(fp);
            }
            updateParentTiledLayoutIfNeeded(fp);
            if (!fp->isType(GO_PROPERTY_VALUE_FIGURE_STR) && !fp->isType(GO_PROPERTY_VALUE_ROOT_STR)
                && !fp->isType(GO_PROPERTY_VALUE_UICONTROL_STR)) {
                GOFigure* fig = fp->getParentFigure();
                if (fig) {
                    fig->setRenderingStateInvalid(true);
                    if (!fig->isDrawLater()) {
                        fig->repaint();
                    }
                }
            }
        } else if (autoLimitsRequested) {
            // Asking for 'auto' on a limit already automatic still recomputes it.
            GOAxis* axis = static_cast<GOAxis*>(fp);
            axis->markStateDirty();
            axis->updateStateIfDirty();
            GOFigure* fig = axis->getParentFigure();
            if (fig) {
                fig->setRenderingStateInvalid(true);
                if (!fig->isDrawLater()) {
                    fig->repaint();
                }
            }
        }
    }

    return ArrayOfVector();
}
//=============================================================================
// Properties whose change must rebuild the static .m polar grid handles.
static bool
isPolarGridProperty(const std::wstring& propname)
{
    static const wstringVector names = { GO_THETA_ZERO_LOCATION_PROPERTY_NAME_STR,
        GO_THETA_DIR_PROPERTY_NAME_STR, GO_R_COLOR_PROPERTY_NAME_STR,
        GO_THETA_COLOR_PROPERTY_NAME_STR, GO_R_GRID_PROPERTY_NAME_STR,
        GO_THETA_GRID_PROPERTY_NAME_STR, GO_R_MINOR_GRID_PROPERTY_NAME_STR,
        GO_THETA_MINOR_GRID_PROPERTY_NAME_STR, GO_R_DIR_PROPERTY_NAME_STR,
        GO_R_AXIS_LOCATION_PROPERTY_NAME_STR, GO_R_MINOR_TICK_PROPERTY_NAME_STR,
        GO_THETA_MINOR_TICK_PROPERTY_NAME_STR };
    return std::find(names.begin(), names.end(), propname) != names.end();
}
//=============================================================================
// Grid lines and labels of a polar axes are static .m handles built once by
// polarRefresh; a dot-set never touches them. Rebuild them through the public
// __polar_refresh__ wrapper. The guard stops the rebuild, which sets axes
// properties itself, from re-entering this path.
static void
refreshPolarGrid(GraphicsObject* axisObject)
{
    static bool inProgress = false;
    if (inProgress || !axisObject) {
        return;
    }
    auto* eval = static_cast<Evaluator*>(NelsonConfiguration::getInstance()->getMainEvaluator());
    if (!eval) {
        return;
    }
    FunctionDefPtr funcDef;
    if (!eval->lookupFunction("__polar_refresh__", funcDef) || !funcDef) {
        return;
    }
    inProgress = true;
    try {
        ArrayOfVector inputs;
        inputs << ArrayOf::graphicsObject(
            findGraphicsObjectHandle(axisObject), axisObject->getClassName());
        funcDef->evaluateFunction(eval, inputs, 0);
    } catch (...) {
    }
    inProgress = false;
}
//=============================================================================
// Properties whose change must rebuild the heatmap's internal primitives.
static bool
isHeatmapChartProperty(const std::wstring& propname)
{
    static const wstringVector names = { GO_COLOR_LIMITS_PROPERTY_NAME_STR,
        GO_COLORBAR_VISIBLE_PROPERTY_NAME_STR, GO_GRID_VISIBLE_PROPERTY_NAME_STR,
        GO_CELL_LABEL_FORMAT_PROPERTY_NAME_STR, GO_CELL_LABEL_COLOR_PROPERTY_NAME_STR,
        GO_MISSING_DATA_LABEL_PROPERTY_NAME_STR, GO_COLOR_METHOD_PROPERTY_NAME_STR,
        GO_FONT_COLOR_PROPERTY_NAME_STR, GO_FONT_SIZE_PROPERTY_NAME_STR,
        GO_TITLE_PROPERTY_NAME_STR, GO_X_LABEL_PROPERTY_NAME_STR,
        GO_Y_LABEL_PROPERTY_NAME_STR };
    return std::find(names.begin(), names.end(), propname) != names.end();
}
//=============================================================================
// A heatmap chart draws its image, grid and cell labels as internal .m handles
// built once; a dot-set never touches them. Rebuild them through the public
// __heatmap_refresh__ wrapper. The guard stops re-entry from the rebuild.
static void
refreshHeatmapChart(GraphicsObject* chartObject)
{
    static bool inProgress = false;
    if (inProgress || !chartObject) {
        return;
    }
    auto* eval = static_cast<Evaluator*>(NelsonConfiguration::getInstance()->getMainEvaluator());
    if (!eval) {
        return;
    }
    FunctionDefPtr funcDef;
    if (!eval->lookupFunction("__heatmap_refresh__", funcDef) || !funcDef) {
        return;
    }
    inProgress = true;
    try {
        ArrayOfVector inputs;
        inputs << ArrayOf::graphicsObject(
            findGraphicsObjectHandle(chartObject), chartObject->getClassName());
        funcDef->evaluateFunction(eval, inputs, 0);
    } catch (...) {
    }
    inProgress = false;
}
//=============================================================================
// Properties whose change must re-lay out the word cloud.
static bool
isWordCloudChartProperty(const std::wstring& propname)
{
    static const wstringVector names = { GO_MAX_DISPLAY_WORDS_PROPERTY_NAME_STR,
        GO_COLOR_PROPERTY_NAME_STR, GO_HIGHLIGHT_COLOR_PROPERTY_NAME_STR,
        GO_SHAPE_PROPERTY_NAME_STR, GO_LAYOUT_NUM_PROPERTY_NAME_STR,
        GO_SIZE_POWER_PROPERTY_NAME_STR, GO_FONT_NAME_PROPERTY_NAME_STR,
        GO_BOX_PROPERTY_NAME_STR, GO_TITLE_PROPERTY_NAME_STR,
        GO_TITLE_FONT_NAME_PROPERTY_NAME_STR };
    return std::find(names.begin(), names.end(), propname) != names.end();
}
//=============================================================================
// A word cloud draws its words as .m text handles laid out once; a dot-set
// never touches them. Rebuild them through the public __wordcloud_refresh__
// wrapper. The guard stops re-entry from the rebuild.
static void
refreshWordCloudChart(GraphicsObject* chartObject)
{
    static bool inProgress = false;
    if (inProgress || !chartObject) {
        return;
    }
    auto* eval = static_cast<Evaluator*>(NelsonConfiguration::getInstance()->getMainEvaluator());
    if (!eval) {
        return;
    }
    FunctionDefPtr funcDef;
    if (!eval->lookupFunction("__wordcloud_refresh__", funcDef) || !funcDef) {
        return;
    }
    inProgress = true;
    try {
        ArrayOfVector inputs;
        inputs << ArrayOf::graphicsObject(
            findGraphicsObjectHandle(chartObject), chartObject->getClassName());
        funcDef->evaluateFunction(eval, inputs, 0);
    } catch (...) {
    }
    inProgress = false;
}
//=============================================================================
static bool
isAutoLimitModeRequest(GraphicsObject* fp, const std::wstring& propname, const ArrayOf& value)
{
    if (!fp->isType(GO_PROPERTY_VALUE_AXES_STR) || !value.isRowVectorCharacterArray()) {
        return false;
    }
    return (propname == GO_X_LIM_MODE_PROPERTY_NAME_STR
               || propname == GO_Y_LIM_MODE_PROPERTY_NAME_STR
               || propname == GO_Z_LIM_MODE_PROPERTY_NAME_STR
               || propname == GO_C_LIM_MODE_PROPERTY_NAME_STR
               || propname == GO_A_LIM_MODE_PROPERTY_NAME_STR)
        && StringHelpers::iequals(value.asWideString(), GO_PROPERTY_VALUE_AUTO_STR);
}
//=============================================================================
// Purely cosmetic properties never move the automatic limits, everything else might.
static bool
canMoveAutoLimits(const std::wstring& propname)
{
    static const wstringVector cosmetic = { GO_COLOR_PROPERTY_NAME_STR,
        GO_LINE_WIDTH_PROPERTY_NAME_STR, GO_LINE_STYLE_PROPERTY_NAME_STR,
        GO_FACE_COLOR_PROPERTY_NAME_STR, GO_EDGE_COLOR_PROPERTY_NAME_STR,
        GO_DISPLAY_NAME_PROPERTY_NAME_STR, GO_TAG_PROPERTY_NAME_STR,
        GO_USER_DATA_PROPERTY_NAME_STR, GO_SELECTED_PROPERTY_NAME_STR };
    return std::find(cosmetic.begin(), cosmetic.end(), propname) == cosmetic.end();
}
//=============================================================================
static bool
hasAutoLimits(GOAxis* axis)
{
    return axis->isAuto(GO_X_LIM_MODE_PROPERTY_NAME_STR)
        || axis->isAuto(GO_Y_LIM_MODE_PROPERTY_NAME_STR)
        || axis->isAuto(GO_Z_LIM_MODE_PROPERTY_NAME_STR)
        || axis->isAuto(GO_C_LIM_MODE_PROPERTY_NAME_STR)
        || axis->isAuto(GO_A_LIM_MODE_PROPERTY_NAME_STR);
}
//=============================================================================
// Titles and labels have the axes as Parent but are not among its Children.
static bool
isLimitsContributor(GOAxis* axis, GraphicsObject* object)
{
    auto* parent = dynamic_cast<GOGObjectsProperty*>(
        object->findProperty(GO_PARENT_PROPERTY_NAME_STR, false));
    auto* children = dynamic_cast<GOGObjectsProperty*>(
        axis->findProperty(GO_CHILDREN_PROPERTY_NAME_STR, false));
    if (!parent || !children || parent->data().empty()) {
        return false;
    }
    if (parent->data()[0] != findGraphicsObjectHandle(axis)) {
        return true;
    }
    std::vector<int64> handles(children->data());
    return std::find(handles.begin(), handles.end(), findGraphicsObjectHandle(object))
        != handles.end();
}
//=============================================================================
static GraphicsObject*
resolveGraphicsObject(nelson_handle handle)
{
    GraphicsObject* fp = nullptr;
    if (handle == HANDLE_ROOT_OBJECT) {
        fp = getGraphicsRootObject();
    } else if (handle >= HANDLE_OFFSET_OBJECT) {
        fp = findGraphicsObject(handle);
    } else {
        fp = static_cast<GraphicsObject*>(findGOFigure(handle));
    }
    if (!fp) {
        Error(L"Nelson:graphics:invalidHandle", L"Invalid handle.");
    }
    return fp;
}
//=============================================================================
static ArrayOf
choicesCellFromProperty(GraphicsObject* fp, const std::wstring& propname)
{
    std::vector<std::wstring> choices;
    GOGenericProperty* hp = fp->findProperty(propname, false);
    if (hp) {
        choices = hp->getDictionary();
    }
    size_t nbChoices = choices.size();
    ArrayOf* elements = static_cast<ArrayOf*>(ArrayOf::allocateArrayOf(NLS_CELL_ARRAY, nbChoices));
    Dimensions dims(nbChoices, nbChoices == 0 ? 0 : 1);
    ArrayOf cell(NLS_CELL_ARRAY, dims, elements);
    for (size_t k = 0; k < nbChoices; ++k) {
        elements[k] = ArrayOf::characterArray(choices[k]);
    }
    return cell;
}
//=============================================================================
static ArrayOfVector
graphics_object_set_introspection(const ArrayOfVector& argIn)
{
    if (!argIn[0].isScalar()) {
        Error(L"Nelson:graphics:expectedScalarGraphicsObject", L"Scalar graphics object expected.");
    }
    const nelson_handle* gobjects = static_cast<const nelson_handle*>(argIn[0].dataPointer());
    GraphicsObject* fp = resolveGraphicsObject(gobjects[0]);
    ArrayOfVector retval;
    if (argIn.size() == 2) {
        std::wstring propname = canonicalObjectPropertyName(fp, argIn[1].asWideString());
        if (!fp->findProperty(propname, false)) {
            Error(L"Nelson:graphics:unrecognizedProperty", L"Unrecognized property: %s", propname);
        }
        retval << choicesCellFromProperty(fp, propname);
        return retval;
    }
    wstringVector fieldnames = fp->getVisibleFieldnames();
    wstringVector settableNames;
    ArrayOfVector values;
    settableNames.reserve(fieldnames.size());
    values.reserve(fieldnames.size());
    for (const auto& name : fieldnames) {
        if (!fp->isWritable(name)) {
            continue;
        }
        settableNames.push_back(name);
        values << choicesCellFromProperty(fp, name);
    }
    retval << ArrayOf::structScalar(settableNames, values);
    return retval;
}
//=============================================================================
static bool
isGridModeSetAfterValue(const std::wstring& propname, const ArrayOf& value,
    const std::vector<std::wstring>& namesSetInCall)
{
    static const std::vector<std::wstring> gridModes
        = { GO_GRID_ALPHA_MODE_PROPERTY_NAME_STR, GO_GRID_COLOR_MODE_PROPERTY_NAME_STR,
              GO_GRID_LINE_WIDTH_MODE_PROPERTY_NAME_STR, GO_MINOR_GRID_ALPHA_MODE_PROPERTY_NAME_STR,
              GO_MINOR_GRID_COLOR_MODE_PROPERTY_NAME_STR,
              GO_MINOR_GRID_LINE_WIDTH_MODE_PROPERTY_NAME_STR };
    if (std::find(gridModes.begin(), gridModes.end(), propname) == gridModes.end()) {
        return false;
    }
    if (!value.isRowVectorCharacterArray() && !value.isScalarStringArray()) {
        return false;
    }
    if (!StringHelpers::iequals(value.asWideString(), GO_PROPERTY_VALUE_AUTO_STR)) {
        return false;
    }
    const std::wstring baseName = propname.substr(0, propname.size() - 4);
    return std::find(namesSetInCall.begin(), namesSetInCall.end(), baseName)
        != namesSetInCall.end();
}
//=============================================================================
static bool
propertyValueAlreadySet(GOGenericProperty* hp, const ArrayOf& value)
{
    return stringPropertyAlreadySet(hp, value) || vectorPropertyAlreadySet(hp, value)
        || logicalPropertyAlreadySet(hp, value) || gobjectsPropertyAlreadySet(hp, value);
}
//=============================================================================
static bool
stringPropertyAlreadySet(GOGenericProperty* hp, const ArrayOf& value)
{
    GOStringProperty* property = dynamic_cast<GOStringProperty*>(hp);
    if (!property || (!value.isRowVectorCharacterArray() && !value.isScalarStringArray())) {
        return false;
    }
    return property->data() == value.asWideString();
}
//=============================================================================
static bool
vectorPropertyAlreadySet(GOGenericProperty* hp, const ArrayOf& value)
{
    GOVectorProperty* property = dynamic_cast<GOVectorProperty*>(hp);
    if (!property || (!value.isNumeric() && !value.isLogical()) || value.isComplex()) {
        return false;
    }
    ArrayOf asDouble(value);
    asDouble.promoteType(NLS_DOUBLE);
    std::vector<double> current = property->data();
    if (current.size() != static_cast<size_t>(asDouble.elementCount())) {
        return false;
    }
    const double* values = static_cast<const double*>(asDouble.dataPointer());
    for (indexType k = 0; k < asDouble.elementCount(); ++k) {
        if (!std::isfinite(values[k]) || current[k] != values[k]) {
            return false;
        }
    }
    return true;
}
//=============================================================================
static bool
logicalPropertyAlreadySet(GOGenericProperty* hp, const ArrayOf& value)
{
    GOScalarLogicalProperty* property = dynamic_cast<GOScalarLogicalProperty*>(hp);
    if (!property || !value.isScalar() || (!value.isLogical() && !value.isNumeric())) {
        return false;
    }
    return property->data() == (value.asDoubleScalar() != 0);
}
//=============================================================================
static bool
gobjectsPropertyAlreadySet(GOGenericProperty* hp, const ArrayOf& value)
{
    GOGObjectsProperty* property = dynamic_cast<GOGObjectsProperty*>(hp);
    if (!property) {
        return false;
    }
    std::vector<int64> current = property->data();
    if (value.isEmpty()) {
        return current.empty();
    }
    if (!value.isGraphicsObject() || current.size() != static_cast<size_t>(value.elementCount())) {
        return false;
    }
    const int64* handles = static_cast<const int64*>(value.dataPointer());
    for (indexType k = 0; k < value.elementCount(); ++k) {
        if (current[k] != handles[k]) {
            return false;
        }
    }
    return true;
}
//=============================================================================
static void
checkVisiblePropertyAppliesToObject(GraphicsObject* fp, const std::wstring& propname)
{
    if (!fp->isType(GO_PROPERTY_VALUE_SUBPLOT_TEXT_STR)) {
        return;
    }
    if (isTiledLayoutSubplotTextFontSizeMode(fp, propname)) {
        return;
    }
    wstringVector fieldnames = fp->getVisibleFieldnames();
    if (std::find(fieldnames.begin(), fieldnames.end(), propname) == fieldnames.end()) {
        Error(L"Nelson:graphics:unrecognizedProperty", L"Unrecognized property: %s", propname);
    }
}
//=============================================================================
static std::wstring
themeNameFromValue(const ArrayOf& value)
{
    if (value.isClassType() && value.classType() == GO_CLASS_NAME_GRAPHICS_THEME_STR) {
        return value.field("BaseColorStyle").asWideString();
    }
    return value.asWideString();
}
//=============================================================================
static void
checkPropertyAppliesToObject(GraphicsObject* fp, const std::wstring& propname)
{
    GOAxis* axis = dynamic_cast<GOAxis*>(fp);
    if (axis && GOAxis::isPolarAxesOnlyProperty(propname) && !axis->isPolarAxes()) {
        Error(L"Nelson:graphics:unrecognizedProperty", L"Unrecognized property: %s", propname);
    }
}
//=============================================================================
static bool
isTiledLayoutSubplotTextFontSizeMode(GraphicsObject* fp, const std::wstring& propname)
{
    if (!fp || !fp->isType(GO_PROPERTY_VALUE_SUBPLOT_TEXT_STR)
        || !StringHelpers::iequals(propname, GO_FONT_SIZE_MODE_PROPERTY_NAME_STR)) {
        return false;
    }
    int64 parentHandle = fp->findGoProperty(GO_PARENT_PROPERTY_NAME_STR);
    GraphicsObject* parent = findGraphicsObject(parentHandle, false);
    return parent && parent->isType(GO_PROPERTY_VALUE_TILED_LAYOUT_STR)
        && fp->findProperty(GO_FONT_SIZE_MODE_PROPERTY_NAME_STR, false);
}
//=============================================================================
static void
setContextMenuProperty(GraphicsObject* fp, ArrayOf value)
{
    GOGObjectsProperty* property
        = static_cast<GOGObjectsProperty*>(fp->findProperty(GO_CONTEXT_MENU_PROPERTY_NAME_STR));
    if (value.isEmpty()) {
        property->set(value);
        return;
    }
    if (!value.isGraphicsObject() || value.elementCount() != 1) {
        Error(L"Nelson:uimenu:expectedContextMenu", L"Context menu graphics object expected.");
    }
    go_handle handle = value.asGraphicsObjectScalar();
    GraphicsObject* contextMenu = graphicsObjectFromHandle(handle, true);
    if (!contextMenu->isType(GO_PROPERTY_VALUE_UICONTEXTMENU_STR)) {
        Error(L"Nelson:uimenu:expectedContextMenu", L"Context menu graphics object expected.");
    }
    property->set(value);
}
//=============================================================================
static bool
isReplaceableTextReferenceProperty(const std::wstring& propname)
{
    return propname == GO_TITLE_PROPERTY_NAME_STR || propname == GO_SUBTITLE_PROPERTY_NAME_STR
        || propname == GO_X_LABEL_PROPERTY_NAME_STR || propname == GO_Y_LABEL_PROPERTY_NAME_STR
        || propname == GO_Z_LABEL_PROPERTY_NAME_STR || propname == GO_LABEL_PROPERTY_NAME_STR;
}
//=============================================================================
static void
setObjectReferenceProperty(GraphicsObject* fp, const std::wstring& propname, ArrayOf value)
{
    GOGObjectsProperty* hp = static_cast<GOGObjectsProperty*>(fp->findProperty(propname));
    std::vector<int64> oldHandles(hp->data());
    hp->set(value);
    std::vector<int64> newHandles(hp->data());
    std::vector<int64> childHandles;
    GOGObjectsProperty* children
        = dynamic_cast<GOGObjectsProperty*>(fp->findProperty(GO_CHILDREN_PROPERTY_NAME_STR, false));
    if (children) {
        childHandles = children->data();
    }
    for (int64 handle : newHandles) {
        if (handle < HANDLE_OFFSET_OBJECT) {
            continue;
        }
        if (std::find(oldHandles.begin(), oldHandles.end(), handle) != oldHandles.end()) {
            if (isReplaceableTextReferenceProperty(propname)
                && std::find(childHandles.begin(), childHandles.end(), handle)
                    != childHandles.end()) {
                GraphicsObject* object = findGraphicsObject(handle, false);
                if (object && object->referenceCount() <= 1) {
                    object->reference();
                }
            }
            continue;
        }
        GraphicsObject* object = findGraphicsObject(handle, false);
        if (object) {
            object->reference();
        }
    }
    for (int64 handle : oldHandles) {
        if (handle < HANDLE_OFFSET_OBJECT) {
            continue;
        }
        if (std::find(newHandles.begin(), newHandles.end(), handle) != newHandles.end()) {
            continue;
        }
        GraphicsObject* object = findGraphicsObject(handle, false);
        if (object) {
            object->dereference();
            if (object->referenceCount() <= 0) {
                deleteGraphicsObject(handle, false, false);
            }
        }
    }
}
//=============================================================================
static void
deleteReplacedTextObjects(
    const std::vector<int64>& oldHandles, const std::vector<int64>& newHandles)
{
    for (int64 handle : oldHandles) {
        if (handle < HANDLE_OFFSET_OBJECT) {
            continue;
        }
        if (std::find(newHandles.begin(), newHandles.end(), handle) != newHandles.end()) {
            continue;
        }
        GraphicsObject* oldObject = findGraphicsObject(handle, false);
        if (oldObject && oldObject->isType(GO_PROPERTY_VALUE_TEXT_STR)) {
            deleteGraphicsObject(handle, false, false);
        }
    }
}
//=============================================================================
static std::wstring
lowerCopy(const std::wstring& value)
{
    std::wstring result(value);
    StringHelpers::to_lower(result);
    return result;
}
//=============================================================================
static std::wstring
canonicalObjectPropertyName(GraphicsObject* fp, const std::wstring& requested)
{
    if (!fp || fp->findProperty(requested, false)) {
        return requested;
    }
    std::wstring lowered = lowerCopy(requested);
    for (const auto& fieldName : fp->fieldnames()) {
        if (lowerCopy(fieldName) == lowered) {
            return fieldName;
        }
    }
    // Fall back to the shortest unambiguous leading-substring match
    // (e.g. set(h,'FaceC','r'), set(gca,'pos',[...])).
    return fp->resolvePartialPropertyName(requested);
}
//=============================================================================
static std::wstring
canonicalLayoutPropertyName(GOTiledChartLayout* layout, const std::wstring& requested)
{
    if (layout->haveProperty(requested)) {
        return requested;
    }
    std::wstring lowered = lowerCopy(requested);
    for (const auto& fieldName : layout->fieldnames()) {
        if (lowerCopy(fieldName) == lowered) {
            return fieldName;
        }
    }
    return requested;
}
//=============================================================================
static std::pair<int, int>
gridSizeFromArray(const ArrayOf& value)
{
    if (!value.isNumeric() || value.isComplex() || value.elementCount() != 2) {
        Error(L"Nelson:tiledlayout:invalidGridSize",
            L"%s must be a two-element positive integer vector.", std::wstring(L"GridSize"));
    }
    std::vector<double> values = value.asDoubleVector();
    int rows = (int)values[0];
    int cols = (int)values[1];
    if (rows < 1 || cols < 1 || values[0] != std::floor(values[0])
        || values[1] != std::floor(values[1])) {
        Error(L"Nelson:tiledlayout:invalidGridSize",
            L"%s must be a two-element positive integer vector.", std::wstring(L"GridSize"));
    }
    return { rows, cols };
}
//=============================================================================
static ArrayOf
normalizeLayoutValue(const std::wstring& propname, ArrayOf value)
{
    if (!value.isRowVectorCharacterArray() && !value.isScalarStringArray()) {
        return value;
    }

    std::wstring lowered = lowerCopy(value.asWideString());
    if (propname == GO_TILE_SPACING_PROPERTY_NAME_STR) {
        if (lowered == L"normal") {
            lowered = GO_PROPERTY_VALUE_LOOSE_STR;
        }
        return ArrayOf::characterArray(lowered);
    }
    if (propname == GO_PADDING_PROPERTY_NAME_STR) {
        if (lowered == L"normal") {
            lowered = GO_PROPERTY_VALUE_LOOSE_STR;
        } else if (lowered == L"none") {
            lowered = GO_PROPERTY_VALUE_TIGHT_STR;
        }
        return ArrayOf::characterArray(lowered);
    }
    if (propname == GO_TILE_INDEXING_PROPERTY_NAME_STR
        || propname == GO_POSITION_CONSTRAINT_PROPERTY_NAME_STR
        || propname == GO_UNITS_PROPERTY_NAME_STR
        || propname == GO_HANDLE_VISIBILITY_PROPERTY_NAME_STR
        || propname == GO_VISIBLE_PROPERTY_NAME_STR) {
        return ArrayOf::characterArray(lowered);
    }
    return value;
}
//=============================================================================
static bool
setTiledLayoutProperty(GraphicsObject* fp, const std::wstring& propname, ArrayOf value)
{
    GOTiledChartLayout* layout = dynamic_cast<GOTiledChartLayout*>(fp);
    if (!layout) {
        return false;
    }
    std::wstring canonical = canonicalLayoutPropertyName(layout, propname);
    if (!layout->haveProperty(canonical)) {
        Error(L"Nelson:graphics:unknownProperty", L"Unknown property: %s", propname);
    }
    if (canonical == GO_TILE_ARRANGEMENT_PROPERTY_NAME_STR) {
        Error(L"Nelson:graphics:readOnlyProperty", L"Property is read-only: %s", canonical);
    }
    if (canonical == GO_GRID_SIZE_PROPERTY_NAME_STR) {
        auto [rows, cols] = gridSizeFromArray(value);
        layout->setGridSizeFromUser(rows, cols);
        return true;
    }
    if (!layout->isWritable(canonical)) {
        Error(L"Nelson:graphics:readOnlyProperty", L"Property is read-only: %s", canonical);
    }
    if (isReplaceableTextReferenceProperty(canonical)
        && dynamic_cast<GOGObjectsProperty*>(layout->findProperty(canonical))) {
        setObjectReferenceProperty(layout, canonical, value);
    } else {
        layout->findProperty(canonical)->set(normalizeLayoutValue(canonical, value));
    }
    return true;
}
//=============================================================================
static void
updateParentTiledLayoutIfNeeded(GraphicsObject* fp)
{
    if (!fp || fp->getType() == GO_PROPERTY_VALUE_TILED_LAYOUT_STR) {
        return;
    }
    GOGObjectsProperty* parent
        = dynamic_cast<GOGObjectsProperty*>(fp->findProperty(GO_PARENT_PROPERTY_NAME_STR, false));
    if (!parent || parent->data().empty()) {
        return;
    }
    int64 parentHandle = parent->data()[0];
    if (parentHandle < HANDLE_OFFSET_OBJECT || isDeletedGraphicsObject(parentHandle)) {
        return;
    }
    GraphicsObject* parentObject = findGraphicsObject(parentHandle, false);
    GOColorBar* parentColorBar = dynamic_cast<GOColorBar*>(parentObject);
    if (parentColorBar && fp->getType() == L"layoutoptions") {
        parentColorBar->updateState();
        return;
    }
    GOLegend* parentLegend = dynamic_cast<GOLegend*>(parentObject);
    if (parentLegend
        && (fp->getType() == L"layoutoptions" || fp->getType() == GO_PROPERTY_VALUE_TEXT_STR)) {
        parentLegend->updateState();
        return;
    }
    if (!parentObject || parentObject->getType() != GO_PROPERTY_VALUE_TILED_LAYOUT_STR) {
        return;
    }
    parentObject->updateState();
}
//=============================================================================
static void
setParent(GraphicsObject* fp, ArrayOf newParent)
{

    go_handle goNewParent = newParent.asGraphicsObjectScalar();
    validateGO(goNewParent);
    go_handle childHandle = findGraphicsObjectHandle(fp);
    if (childHandle == HANDLE_UNASSIGNED_OBJECT) {
        Error(L"Nelson:graphics:invalidHandle", L"Invalid handle.");
    }
    if (fp->isType(GO_PROPERTY_VALUE_UICONTEXTMENU_STR)
        || fp->isType(GO_PROPERTY_VALUE_UIMENU_STR)) {
        setGraphicsMenuParent(childHandle, goNewParent);
        return;
    }

    GOGObjectsProperty* hParent
        = dynamic_cast<GOGObjectsProperty*>(fp->findProperty(GO_PARENT_PROPERTY_NAME_STR, false));
    if (!hParent) {
        return;
    }
    checkUIParentChange(fp, newParent);
    std::vector<int64> oldParents(hParent->data());
    hParent->set(newParent);
    if (oldParents.size() == 1 && oldParents[0] != goNewParent) {
        removeChildReference(oldParents[0], childHandle);
    }
    addChildReference(goNewParent, childHandle);
}
//=============================================================================
static void
addChildReference(go_handle parentHandle, go_handle childHandle)
{
    if (parentHandle == HANDLE_ROOT_OBJECT) {
        return;
    }
    GraphicsObject* parent = graphicsObjectFromHandle(parentHandle, false);
    if (!parent) {
        return;
    }
    GOGObjectsProperty* children = dynamic_cast<GOGObjectsProperty*>(
        parent->findProperty(GO_CHILDREN_PROPERTY_NAME_STR, false));
    if (!children) {
        return;
    }
    std::vector<int64> childHandles(children->data());
    if (std::find(childHandles.begin(), childHandles.end(), childHandle) == childHandles.end()) {
        childHandles.push_back(childHandle);
        children->data(childHandles);
        parent->updateState();
    }
}
//=============================================================================
static void
removeChildReference(go_handle parentHandle, go_handle childHandle)
{
    if (parentHandle == HANDLE_ROOT_OBJECT) {
        return;
    }
    GraphicsObject* parent = graphicsObjectFromHandle(parentHandle, false);
    if (!parent) {
        return;
    }
    GOGObjectsProperty* children = dynamic_cast<GOGObjectsProperty*>(
        parent->findProperty(GO_CHILDREN_PROPERTY_NAME_STR, false));
    if (!children) {
        return;
    }
    std::vector<int64> childHandles(children->data());
    auto newEnd = std::remove(childHandles.begin(), childHandles.end(), childHandle);
    if (newEnd != childHandles.end()) {
        childHandles.erase(newEnd, childHandles.end());
        children->data(childHandles);
        parent->updateState();
    }
}
//=============================================================================
static void
setChildren(GraphicsObject* fp, ArrayOf childrenToAdd)
{
    const int64* dp = static_cast<const int64*>(childrenToAdd.dataPointer());
    for (int i = 0; i < childrenToAdd.elementCount(); i++) {
        validateGO(dp[i]);
    }
    GraphicsObject* gp = nullptr;
    GOGObjectsProperty* hp
        = static_cast<GOGObjectsProperty*>(fp->findProperty(GO_CHILDREN_PROPERTY_NAME_STR));
    std::vector<int64> my_children(hp->data());
    for (int i = 0; i < my_children.size(); i++) {
        int64 handle = my_children[i];
        if (handle >= HANDLE_OFFSET_OBJECT) {
            GraphicsObject* gp = findGraphicsObject(handle);
            gp->dereference();
        }
    }
    for (int i = 0; i < childrenToAdd.elementCount(); i++) {
        int64 handle = dp[i];
        if (handle >= HANDLE_OFFSET_OBJECT) {
            GraphicsObject* gp = findGraphicsObject(handle);
            gp->reference();
        }
    }
    for (int i = 0; i < my_children.size(); i++) {
        int64 handle = my_children[i];
        if (handle >= HANDLE_OFFSET_OBJECT) {
            gp = findGraphicsObject(handle);
            if (gp->referenceCount() <= 0) {
                // A child dropped here is destroyed without going through
                // deleteGraphicsObject, so the figure has to forget it by hand.
                clearFigureCurrentReferences(handle);
                freeGraphicsObject(handle);
                delete gp;
            }
        }
    }
    // Assigning Children reorders them. A stacking container reports its children top first but
    // stores them back to front, so the list is reversed before it is stored.
    if (stacksChildren(fp) && childrenToAdd.elementCount() > 1) {
        std::vector<int64> ordered;
        ordered.reserve(childrenToAdd.elementCount());
        for (indexType k = childrenToAdd.elementCount(); k > 0; --k) {
            ordered.push_back(dp[k - 1]);
        }
        hp->set(graphicsObjectVector(ordered));
        return;
    }
    hp->set(childrenToAdd);
}
//=============================================================================
} // namespace Nelson::GraphicsGateway
//=============================================================================
