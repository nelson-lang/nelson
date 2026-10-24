//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <memory>
#include <set>
#include <vector>
#include "FigurePersistence.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "NelsonException.hpp"
#include "i18n.hpp"
#include "GOAxis.hpp"
#include "GOFigure.hpp"
#include "GOFiguresManager.hpp"
#include "GOGObjectsProperty.hpp"
#include "GOHelpers.hpp"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "GraphicsObject.hpp"
#include "GraphicsObjectFactory.hpp"
#include "UIControlBuilder.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static ArrayOf
makeCell(const ArrayOfVector& values)
{
    const indexType count = static_cast<indexType>(values.size());
    if (count == 0) {
        return ArrayOf(NLS_CELL_ARRAY, Dimensions(0, 1), nullptr);
    }
    std::unique_ptr<ArrayOf[]> elements(new ArrayOf[count]);
    for (indexType k = 0; k < count; ++k) {
        elements[k] = values[k];
    }
    ArrayOf result(NLS_CELL_ARRAY, Dimensions(count, 1), elements.get());
    elements.release();
    return result;
}
//=============================================================================
static bool
isSerializableProperty(GraphicsObject* object, const std::wstring& name)
{
    return object->isWritable(name) && !isGraphicsTreeLinkProperty(name);
}
//=============================================================================
static bool
isEmptyGraphicsReference(const ArrayOf& value)
{
    return value.isGraphicsObject() && value.elementCount() == 0;
}
//=============================================================================
// Axes always carry Title/Subtitle/X-Y-ZLabel text objects, so that
// ax.Title.String = 'x' works before title() has run. An empty one holds nothing
// worth saving. Skipping it keeps a reopened figure on the same path as the
// original: created fresh, then synced.
static bool
isEmptyTextReference(GraphicsObject* reference)
{
    return reference != nullptr && reference->isType(GO_PROPERTY_VALUE_TEXT_STR)
        && reference->findStringProperty(GO_STRING_PROPERTY_NAME_STR).empty();
}
//=============================================================================
static bool
isDescendantHandle(GraphicsObject* object, int64 target)
{
    GOGObjectsProperty* children = static_cast<GOGObjectsProperty*>(
        object->findProperty(GO_CHILDREN_PROPERTY_NAME_STR, false));
    if (!children) {
        return false;
    }
    for (int64 child : children->data()) {
        if (child == target) {
            return true;
        }
        if (child >= HANDLE_OFFSET_OBJECT && !isDeletedGraphicsObject(child)) {
            GraphicsObject* childObject = graphicsObjectFromHandle(child, true);
            if (isDescendantHandle(childObject, target)) {
                return true;
            }
        }
    }
    return false;
}
//=============================================================================
static bool
isTreeAliasReference(GraphicsObject* object, const ArrayOf& value)
{
    if (!value.isGraphicsObject() || isEmptyGraphicsReference(value)) {
        return false;
    }
    const auto* handles = static_cast<const nelson_handle*>(value.dataPointer());
    bool foundReference = false;
    for (indexType k = 0; k < value.elementCount(); ++k) {
        if (handles[k] == HANDLE_ROOT_OBJECT || isDeletedGraphicsObject(handles[k])) {
            continue;
        }
        foundReference = true;
        if (!isDescendantHandle(object, handles[k])) {
            return false;
        }
    }
    return foundReference;
}
//=============================================================================
static ArrayOf
serializeObject(int64 handle, std::set<int64>& active);
//=============================================================================
static ArrayOf
serializeReferences(GraphicsObject* owner, const wstringVector& names, const ArrayOfVector& values,
    std::set<int64>& active)
{
    wstringVector refNames;
    ArrayOfVector refValues;
    for (size_t k = 0; k < names.size(); ++k) {
        if (!values[k].isGraphicsObject() || isEmptyGraphicsReference(values[k])) {
            continue;
        }
        const auto* handles = static_cast<const nelson_handle*>(values[k].dataPointer());
        ArrayOfVector refs;
        for (indexType p = 0; p < values[k].elementCount(); ++p) {
            if (handles[p] == HANDLE_ROOT_OBJECT || isDeletedGraphicsObject(handles[p])
                || active.find(handles[p]) != active.end()) {
                continue;
            }
            GraphicsObject* reference = graphicsObjectFromHandle(handles[p], true);
            if (reference && !isEmptyTextReference(reference)
                && canCreateGraphicsObjectForType(reference->getType(), owner)) {
                refs.push_back(serializeObject(handles[p], active));
            }
        }
        if (!refs.empty()) {
            refNames.push_back(names[k]);
            refValues.push_back(makeCell(refs));
        }
    }
    return ArrayOf::structScalar(refNames, refValues);
}
//=============================================================================
static ArrayOf
serializeChildren(GraphicsObject* object, std::set<int64>& active)
{
    if (object->isType(GO_PROPERTY_VALUE_STACKED_LINE_CHART_STR)) {
        return makeCell({});
    }
    GOGObjectsProperty* children = static_cast<GOGObjectsProperty*>(
        object->findProperty(GO_CHILDREN_PROPERTY_NAME_STR, false));
    ArrayOfVector values;
    if (children) {
        for (int64 child : children->data()) {
            if (!isDeletedGraphicsObject(child)) {
                values.push_back(serializeObject(child, active));
            }
        }
    }
    return makeCell(values);
}
//=============================================================================
static ArrayOf
serializeObject(int64 handle, std::set<int64>& active)
{
    GraphicsObject* object = graphicsObjectFromHandle(handle, true);
    active.insert(handle);

    wstringVector propertyNames;
    ArrayOfVector propertyValues;
    wstringVector referenceNames;
    ArrayOfVector referenceValues;
    for (const auto& name : object->fieldnames()) {
        if (!isSerializableProperty(object, name)) {
            continue;
        }
        GOGenericProperty* property = object->findProperty(name, false);
        if (!property) {
            continue;
        }
        ArrayOf value = property->get();
        if (value.isGraphicsObject()) {
            if (!isTreeAliasReference(object, value)) {
                referenceNames.push_back(name);
                referenceValues.push_back(value);
            }
        } else if (containsGraphicsRuntimeValue(value)) {
            continue;
        } else {
            propertyNames.push_back(name);
            propertyValues.push_back(value);
        }
    }

    wstringVector fields = { L"Format", L"Type", L"Properties", L"References", L"Children" };
    ArrayOfVector values;
    values << ArrayOf::characterArray(L"NelsonFigObject");
    values << ArrayOf::characterArray(object->getType());
    values << ArrayOf::structScalar(propertyNames, propertyValues);
    values << serializeReferences(object, referenceNames, referenceValues, active);
    values << serializeChildren(object, active);
    active.erase(handle);
    return ArrayOf::structScalar(fields, values);
}
//=============================================================================
ArrayOf
serializeFiguresToFigPayload(const ArrayOf& figuresArray)
{
    if (!figuresArray.isGraphicsObject()) {
        Error(L"Nelson:savefig:expectedFigure", L"Expected figure graphics object(s).");
    }

    const auto* handles = static_cast<const nelson_handle*>(figuresArray.dataPointer());
    ArrayOfVector figures;
    for (indexType k = 0; k < figuresArray.elementCount(); ++k) {
        GraphicsObject* object = graphicsObjectFromHandle(handles[k], true);
        if (!object->isType(GO_PROPERTY_VALUE_FIGURE_STR)) {
            Error(L"Nelson:savefig:expectedFigure", L"Expected figure graphics object(s).");
        }
        std::set<int64> active;
        figures.push_back(serializeObject(handles[k], active));
    }

    wstringVector fields = { L"Format", L"Version", L"Figures" };
    ArrayOfVector values;
    values << ArrayOf::characterArray(L"NelsonFig");
    values << ArrayOf::doubleScalar(1);
    values << makeCell(figures);
    return ArrayOf::structScalar(fields, values);
}
//=============================================================================
static std::wstring
toWidePropertyName(const std::string& name)
{
    return std::wstring(name.begin(), name.end());
}
//=============================================================================
static std::vector<ArrayOf>
cellElements(const ArrayOf& cell)
{
    std::vector<ArrayOf> values;
    if (!cell.isCell()) {
        return values;
    }
    const auto* ptr = static_cast<const ArrayOf*>(cell.dataPointer());
    for (indexType k = 0; k < cell.elementCount(); ++k) {
        values.push_back(ptr[k]);
    }
    return values;
}
//=============================================================================
static void
appendChild(int64 parentHandle, int64 childHandle)
{
    GraphicsObject* parent = graphicsObjectFromHandle(parentHandle, true);
    GOGObjectsProperty* children = static_cast<GOGObjectsProperty*>(
        parent->findProperty(GO_CHILDREN_PROPERTY_NAME_STR, false));
    if (!children) {
        return;
    }
    std::vector<int64> values = children->data();
    values.push_back(childHandle);
    children->data(values);
}
//=============================================================================
static void
setParent(GraphicsObject* target, int64 parentHandle)
{
    GOGObjectsProperty* parent = static_cast<GOGObjectsProperty*>(
        target->findProperty(GO_PARENT_PROPERTY_NAME_STR, false));
    if (parent) {
        parent->data({ parentHandle });
    }
}
//=============================================================================
static bool
isAxesLikeObject(GraphicsObject* object)
{
    return object
        && (object->isType(GO_PROPERTY_VALUE_AXES_STR)
            || object->isType(GO_PROPERTY_VALUE_POLAR_AXES_STR));
}
//=============================================================================
static int64
firstAxesLikeChild(GraphicsObject* object)
{
    if (isAxesLikeObject(object)) {
        return findGraphicsObjectHandle(object);
    }
    GOGObjectsProperty* children = static_cast<GOGObjectsProperty*>(
        object->findProperty(GO_CHILDREN_PROPERTY_NAME_STR, false));
    if (!children) {
        return -1;
    }
    for (int64 child : children->data()) {
        if (child >= HANDLE_OFFSET_OBJECT && !isDeletedGraphicsObject(child)) {
            GraphicsObject* childObject = graphicsObjectFromHandle(child, false);
            int64 axesHandle = childObject ? firstAxesLikeChild(childObject) : -1;
            if (axesHandle >= HANDLE_OFFSET_OBJECT) {
                return axesHandle;
            }
        }
    }
    return -1;
}
//=============================================================================
static void
restoreCurrentAxes(int64 figureHandle)
{
    GOFigure* figure = findGOFigure(figureHandle);
    if (!figure) {
        return;
    }
    int64 currentAxes = figure->findGoProperty(GO_CURRENT_AXES_PROPERTY_NAME_STR);
    if (currentAxes >= HANDLE_OFFSET_OBJECT && !isDeletedGraphicsObject(currentAxes)) {
        return;
    }
    int64 axesHandle = firstAxesLikeChild(figure);
    if (axesHandle >= HANDLE_OFFSET_OBJECT) {
        figure->setGoProperty(GO_CURRENT_AXES_PROPERTY_NAME_STR, axesHandle);
    }
}
//=============================================================================
static int64
createObject(const std::wstring& type, int64 parentHandle, bool appendToParent)
{
    if (type == GO_PROPERTY_VALUE_FIGURE_STR) {
        return createNewFigure(false);
    }
    if (type == GO_PROPERTY_VALUE_UICONTROL_STR) {
        ArrayOfVector args;
        args << ArrayOf::graphicsObject(parentHandle);
        return UIControlBuilder(args);
    }
    std::unique_ptr<GraphicsObject> target;
    GraphicsObject* parent = graphicsObjectFromHandle(parentHandle, true);
    target.reset(newGraphicsObjectForType(type, parent));
    if (!target) {
        Error(L"Nelson:graphics:unsupportedType", L"Unsupported graphics object type.");
    }
    int64 handle = assignGraphicsObject(target.get());
    target.release();
    GraphicsObject* object = graphicsObjectFromHandle(handle, true);
    setParent(object, parentHandle);
    if (appendToParent) {
        appendChild(parentHandle, handle);
    }
    if (object->isType(GO_PROPERTY_VALUE_AXES_STR)) {
        static_cast<GOAxis*>(object)->restoreToolbar(handle);
    }
    return handle;
}
//=============================================================================
static void
applyProperties(GraphicsObject* target, const ArrayOf& properties)
{
    if (!properties.isStruct() || !properties.isScalar()) {
        return;
    }
    for (const auto& name : properties.fieldNames()) {
        std::wstring wideName = toWidePropertyName(name);
        GOGenericProperty* property = target->findProperty(wideName, false);
        if (!property || !target->isWritable(wideName)) {
            continue;
        }
        try {
            ArrayOf value = properties.field(name);
            if (target->isType(GO_PROPERTY_VALUE_FIGURE_STR)
                && wideName == GO_THEME_PROPERTY_NAME_STR) {
                static_cast<GOFigure*>(target)->setTheme(value.asWideString(), true);
            } else if (target->isType(GO_PROPERTY_VALUE_FIGURE_STR)
                && wideName == GO_THEME_MODE_PROPERTY_NAME_STR) {
                static_cast<GOFigure*>(target)->setThemeMode(value.asWideString());
            } else {
                property->set(value);
            }
        } catch (const NelsonException&) {
        }
    }
}
//=============================================================================
static bool
isModePropertyName(const std::wstring& name)
{
    constexpr size_t suffixLength = 4;
    return name.length() > suffixLength
        && name.compare(name.length() - suffixLength, suffixLength, L"Mode") == 0;
}
//=============================================================================
static void
applyModeProperties(GraphicsObject* target, const ArrayOf& properties)
{
    if (!properties.isStruct() || !properties.isScalar()) {
        return;
    }
    for (const auto& name : properties.fieldNames()) {
        std::wstring wideName = toWidePropertyName(name);
        if (!isModePropertyName(wideName)) {
            continue;
        }
        GOGenericProperty* property = target->findProperty(wideName, false);
        if (!property || !target->isWritable(wideName)) {
            continue;
        }
        try {
            property->set(properties.field(name));
        } catch (const NelsonException&) {
        }
    }
}
//=============================================================================
static int64
restoreObject(const ArrayOf& record, int64 parentHandle, bool appendToParent,
    const std::wstring& figureVisibleAction = L"");
//=============================================================================
static void
applyReferences(GraphicsObject* target, const ArrayOf& references, int64 targetHandle)
{
    if (!references.isStruct() || !references.isScalar()) {
        return;
    }
    for (const auto& name : references.fieldNames()) {
        GOGObjectsProperty* property = dynamic_cast<GOGObjectsProperty*>(
            target->findProperty(toWidePropertyName(name), false));
        if (!property) {
            continue;
        }
        std::vector<int64> handles;
        for (const auto& childRecord : cellElements(references.field(name))) {
            handles.push_back(restoreObject(childRecord, targetHandle, false));
        }
        property->data(handles);
    }
}
//=============================================================================
static void
restoreChildren(GraphicsObject* target, const ArrayOf& children, int64 targetHandle)
{
    if (target->isType(GO_PROPERTY_VALUE_TILED_LAYOUT_STR)) {
        int64 figureHandle = -1;
        GOGObjectsProperty* parent = static_cast<GOGObjectsProperty*>(
            target->findProperty(GO_PARENT_PROPERTY_NAME_STR, false));
        if (parent && !parent->data().empty() && parent->data()[0] < HANDLE_OFFSET_OBJECT) {
            figureHandle = parent->data()[0];
        }
        if (figureHandle < 0) {
            Error(L"Nelson:openfig:invalidFile", L"Invalid Nelson figure file.");
        }
        for (const auto& childRecord : cellElements(children)) {
            int64 childHandle = restoreObject(childRecord, figureHandle, true);
            appendChild(targetHandle, childHandle);
        }
        target->updateState();
        return;
    }
    for (const auto& childRecord : cellElements(children)) {
        restoreObject(childRecord, targetHandle, true);
    }
    target->updateState();
}
//=============================================================================
static void
applyFigureVisibleAction(GraphicsObject* target, const std::wstring& visibleAction)
{
    if (!target || !target->isType(GO_PROPERTY_VALUE_FIGURE_STR) || visibleAction.empty()) {
        return;
    }
    GOGenericProperty* property = target->findProperty(GO_VISIBLE_PROPERTY_NAME_STR, false);
    if (!property) {
        return;
    }
    property->set(ArrayOf::characterArray(
        visibleAction == L"visible" ? GO_PROPERTY_VALUE_ON_STR : GO_PROPERTY_VALUE_OFF_STR));
}
//=============================================================================
static int64
restoreObject(const ArrayOf& record, int64 parentHandle, bool appendToParent,
    const std::wstring& figureVisibleAction)
{
    if (!record.isStruct() || !record.isScalar() || !record.isField("Type")) {
        Error(L"Nelson:openfig:invalidFile", L"Invalid Nelson figure file.");
    }
    std::wstring type = record.field("Type").asWideString();
    int64 handle = createObject(type, parentHandle, appendToParent);
    GraphicsObject* target = graphicsObjectFromHandle(handle, true);
    if (record.isField("Properties")) {
        applyProperties(target, record.field("Properties"));
    }
    applyFigureVisibleAction(target, figureVisibleAction);
    if (record.isField("References")) {
        applyReferences(target, record.field("References"), handle);
    }
    if (record.isField("Children")) {
        restoreChildren(target, record.field("Children"), handle);
    } else {
        target->updateState();
    }
    if (record.isField("Properties")) {
        applyModeProperties(target, record.field("Properties"));
        target->updateState();
    }
    return handle;
}
//=============================================================================
static ArrayOf
handlesToArray(const std::vector<int64>& handles)
{
    if (handles.empty()) {
        return ArrayOf(NLS_GO_HANDLE, Dimensions(0, 1), nullptr);
    }
    auto* data = static_cast<nelson_handle*>(
        ArrayOf::allocateArrayOf(NLS_GO_HANDLE, static_cast<indexType>(handles.size())));
    for (indexType k = 0; k < static_cast<indexType>(handles.size()); ++k) {
        data[k] = handles[k];
    }
    return ArrayOf(NLS_GO_HANDLE, Dimensions(static_cast<indexType>(handles.size()), 1), data);
}
//=============================================================================
static std::vector<int64>
findFiguresByFileName(const std::wstring& filename)
{
    std::vector<int64> matches;
    for (int64 handle : getFigureGraphicsObjects()) {
        GOFigure* figure = findGOFigure(handle);
        if (!figure || !figure->haveProperty(GO_FILE_NAME_PROPERTY_NAME_STR)) {
            continue;
        }
        if (figure->findStringProperty(GO_FILE_NAME_PROPERTY_NAME_STR) == filename) {
            matches.push_back(handle);
        }
    }
    return matches;
}
//=============================================================================
static void
setFigureStringProperty(int64 handle, const std::wstring& propertyName, const std::wstring& value)
{
    GOFigure* figure = findGOFigure(handle);
    if (!figure || !figure->haveProperty(propertyName)) {
        return;
    }
    try {
        figure->findProperty(propertyName)->set(ArrayOf::characterArray(value));
        figure->updateState(true);
    } catch (const NelsonException&) {
    }
}
//=============================================================================
static void
showFigureIfNeeded(int64 handle, const std::wstring& visibleAction)
{
    GOFigure* figure = findGOFigure(handle);
    if (!figure) {
        return;
    }
    std::wstring visible = visibleAction;
    if (visible.empty() && figure->haveProperty(GO_VISIBLE_PROPERTY_NAME_STR)) {
        visible = figure->findStringProperty(GO_VISIBLE_PROPERTY_NAME_STR);
    }
    if (visible == GO_PROPERTY_VALUE_ON_STR || visible == L"visible") {
        selectFigure(handle, true);
    }
}
//=============================================================================
ArrayOf
restoreFiguresFromFigPayload(const ArrayOf& payload, const std::wstring& filename,
    const std::wstring& visibleAction, bool reuse)
{
    if (!payload.isStruct() || !payload.isScalar() || !payload.isField("Figures")) {
        Error(L"Nelson:openfig:invalidFile", L"Invalid Nelson figure file.");
    }
    if (reuse) {
        std::vector<int64> existing = findFiguresByFileName(filename);
        if (!existing.empty()) {
            for (int64 handle : existing) {
                if (!visibleAction.empty()) {
                    setFigureStringProperty(handle, GO_VISIBLE_PROPERTY_NAME_STR,
                        visibleAction == L"visible" ? GO_PROPERTY_VALUE_ON_STR
                                                    : GO_PROPERTY_VALUE_OFF_STR);
                }
                showFigureIfNeeded(handle, visibleAction);
            }
            return handlesToArray(existing);
        }
    }

    std::vector<int64> handles;
    for (const auto& figureRecord : cellElements(payload.field("Figures"))) {
        int64 handle = restoreObject(figureRecord, HANDLE_ROOT_OBJECT, false, visibleAction);
        restoreCurrentAxes(handle);
        setFigureStringProperty(handle, GO_FILE_NAME_PROPERTY_NAME_STR, filename);
        if (!visibleAction.empty()) {
            setFigureStringProperty(handle, GO_VISIBLE_PROPERTY_NAME_STR,
                visibleAction == L"visible" ? GO_PROPERTY_VALUE_ON_STR : GO_PROPERTY_VALUE_OFF_STR);
        }
        showFigureIfNeeded(handle, visibleAction);
        handles.push_back(handle);
    }
    return handlesToArray(handles);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
