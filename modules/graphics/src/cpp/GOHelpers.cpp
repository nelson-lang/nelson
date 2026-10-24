//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <limits>
#include <algorithm>
#include <cwctype>
#include "GOHelpers.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "GOList.hpp"
#include "GOFiguresManager.hpp"
#include "GORoot.hpp"
#include "GOGObjectsProperty.hpp"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "StringHelpers.hpp"
#include "GOCallbackProperty.hpp"
#include "GOColorBar.hpp"
#include "GOBubbleLegend.hpp"
#include "GOLegend.hpp"
#include "GOStringOnOffProperty.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static GOList<GraphicsObject*> objectset;
//=============================================================================
static bool
isCharacterMatrix(const ArrayOf& arg);
//=============================================================================
static wstringVector
characterMatrixRows(const ArrayOf& arg);
//=============================================================================
static void
clearAxesToolbarReference(GraphicsObject* object, int64 handle)
{
    if (!object || !object->isType(GO_PROPERTY_VALUE_AXES_TOOLBAR_STR)) {
        return;
    }
    GOGObjectsProperty* parent = static_cast<GOGObjectsProperty*>(
        object->findProperty(GO_PARENT_PROPERTY_NAME_STR, false));
    if (!parent || parent->data().empty()) {
        return;
    }
    int64 parentHandle = parent->data()[0];
    if (parentHandle < HANDLE_OFFSET_OBJECT) {
        return;
    }
    GraphicsObject* parentObject = findGraphicsObject(parentHandle, false);
    if (!parentObject || !parentObject->isType(GO_PROPERTY_VALUE_AXES_STR)) {
        return;
    }
    GOGObjectsProperty* toolbar = static_cast<GOGObjectsProperty*>(
        parentObject->findProperty(GO_TOOLBAR_PROPERTY_NAME_STR, false));
    if (!toolbar || toolbar->data().empty() || toolbar->data()[0] != handle) {
        return;
    }
    toolbar->data({});
}
//=============================================================================
static void
deleteColorBarsForPeer(int64 peerHandle)
{
    std::vector<int64> figures = getFigureGraphicsObjects();
    for (int64 figureHandle : figures) {
        GOFigure* figure = findGOFigure(figureHandle);
        if (!figure) {
            continue;
        }
        GOGObjectsProperty* children = static_cast<GOGObjectsProperty*>(
            figure->findProperty(GO_CHILDREN_PROPERTY_NAME_STR, false));
        if (!children) {
            continue;
        }
        std::vector<int64> childHandles(children->data());
        for (int64 child : childHandles) {
            GraphicsObject* object = findGraphicsObject(child, false);
            GOColorBar* colorBar = dynamic_cast<GOColorBar*>(object);
            if (colorBar && colorBar->getPeerAxesHandle() == peerHandle) {
                deleteGraphicsObject(child, false, true);
            }
        }
    }
}
//=============================================================================
static void
deleteLegendsForPeer(int64 peerHandle)
{
    std::vector<int64> figures = getFigureGraphicsObjects();
    for (int64 figureHandle : figures) {
        GOFigure* figure = findGOFigure(figureHandle);
        if (!figure) {
            continue;
        }
        GOGObjectsProperty* children = static_cast<GOGObjectsProperty*>(
            figure->findProperty(GO_CHILDREN_PROPERTY_NAME_STR, false));
        if (!children) {
            continue;
        }
        std::vector<int64> childHandles(children->data());
        for (int64 child : childHandles) {
            GraphicsObject* object = findGraphicsObject(child, false);
            GOLegend* legend = dynamic_cast<GOLegend*>(object);
            GOBubbleLegend* bubbleLegend = dynamic_cast<GOBubbleLegend*>(object);
            if ((legend && legend->getPeerAxesHandle() == peerHandle)
                || (bubbleLegend && bubbleLegend->getPeerAxesHandle() == peerHandle)) {
                deleteGraphicsObject(child, false, true);
            }
        }
    }
}
//=============================================================================
static void
restoreColorBarPeerPosition(GraphicsObject* object)
{
    GOColorBar* colorBar = dynamic_cast<GOColorBar*>(object);
    if (colorBar) {
        colorBar->restorePeerAxesPosition();
    }
}
//=============================================================================
static void
restoreLegendPeerPosition(GraphicsObject* object)
{
    GOLegend* legend = dynamic_cast<GOLegend*>(object);
    if (legend) {
        legend->restorePeerAxesPosition();
    }
    GOBubbleLegend* bubbleLegend = dynamic_cast<GOBubbleLegend*>(object);
    if (bubbleLegend) {
        bubbleLegend->restorePeerAxesPosition();
    }
}
//=============================================================================
static void
clearLegendPeerReference(GraphicsObject* object)
{
    GOLegend* legend = dynamic_cast<GOLegend*>(object);
    if (!legend) {
        return;
    }
    int64 peerHandle = legend->getPeerAxesHandle();
    if (peerHandle < HANDLE_OFFSET_OBJECT || isDeletedGraphicsObject(peerHandle)) {
        return;
    }
    GraphicsObject* peer = findGraphicsObject(peerHandle, false);
    if (!peer) {
        return;
    }
    GOGObjectsProperty* legendProperty
        = static_cast<GOGObjectsProperty*>(peer->findProperty(GO_LEGEND_PROPERTY_NAME_STR, false));
    if (legendProperty) {
        legendProperty->data({});
    }
}
//=============================================================================
void
clearFigureCurrentReferences(int64 handle)
{
    std::vector<int64> figures = getFigureGraphicsObjects();
    for (int64 figureHandle : figures) {
        GOFigure* figure = findGOFigure(figureHandle);
        if (!figure) {
            continue;
        }
        for (const auto& propertyName :
            { GO_CURRENT_OBJECT_PROPERTY_NAME_STR, GO_CURRENT_AXES_PROPERTY_NAME_STR }) {
            GOGObjectsProperty* property
                = static_cast<GOGObjectsProperty*>(figure->findProperty(propertyName, false));
            if (property && !property->data().empty() && property->data()[0] == handle) {
                property->data({});
            }
        }
    }
}
//=============================================================================
static void
clearParentInternalObjectReference(GraphicsObject* object, int64 handle)
{
    if (!object) {
        return;
    }
    GOGObjectsProperty* parent = static_cast<GOGObjectsProperty*>(
        object->findProperty(GO_PARENT_PROPERTY_NAME_STR, false));
    if (!parent || parent->data().empty()) {
        return;
    }
    int64 parentHandle = parent->data()[0];
    GraphicsObject* parentObject = nullptr;
    if (parentHandle >= HANDLE_OFFSET_OBJECT) {
        parentObject = findGraphicsObject(parentHandle, false);
    } else if (parentHandle != HANDLE_ROOT_OBJECT && parentHandle != 0) {
        parentObject = static_cast<GraphicsObject*>(findGOFigure(parentHandle));
    }
    if (!parentObject) {
        return;
    }
    for (const auto& propertyName :
        { GO_TITLE_PROPERTY_NAME_STR, GO_SUBTITLE_PROPERTY_NAME_STR, GO_X_LABEL_PROPERTY_NAME_STR,
            GO_Y_LABEL_PROPERTY_NAME_STR, GO_Z_LABEL_PROPERTY_NAME_STR, GO_LABEL_PROPERTY_NAME_STR,
            GO_LAYOUT_PROPERTY_NAME_STR }) {
        GOGObjectsProperty* property
            = dynamic_cast<GOGObjectsProperty*>(parentObject->findProperty(propertyName, false));
        if (property && !property->data().empty() && property->data()[0] == handle) {
            property->data({});
        }
    }
}
//=============================================================================
bool
isDeletedGraphicsObject(int64 handle)
{
    if (isUnassignedGraphicsObject(handle)) {
        return true;
    }
    if (handle == HANDLE_ROOT_OBJECT) {
        return false;
    }
    if (handle >= HANDLE_OFFSET_OBJECT) {
        return (findGraphicsObject(handle, false) == nullptr);
    }
    return (getFigure(handle) == nullptr);
}
//=============================================================================
bool
isUnassignedGraphicsObject(int64 handle)
{
    return handle == HANDLE_UNASSIGNED_OBJECT;
}
//=============================================================================
void
freeGraphicsObject(int64 handle)
{
    objectset.deleteGO(handle - HANDLE_OFFSET_OBJECT);
}
//=============================================================================
void
checkIdValidity(int64 id)
{
    bool isValidId = (id >= 0 && id < (int64)std::numeric_limits<int>::max());
    if (!isValidId) {
        Error("Nelson:figure:invalidId", "Invalid figure id.");
    }
}
//=============================================================================
ArrayOf
uniformizeStringVector(const ArrayOf& arg, wstringVector& asWideStringVector, bool pipeDelimited)
{
    if (arg.isRowVectorCharacterArray()) {
        std::wstring str = arg.asWideString();
        if (pipeDelimited && StringHelpers::contains(str, L"|")) {
            Tokenize(str, asWideStringVector, L"|");
        } else {
            asWideStringVector.push_back(str);
        }
    } else if (isCharacterMatrix(arg)) {
        asWideStringVector = characterMatrixRows(arg);
    } else if (arg.isStringArray() || arg.isCellArrayOfCharacterVectors()) {
        asWideStringVector = arg.asWideStringVector();
    } else if (arg.isEmpty()) {
        // Accept any empty value (e.g. []) as an empty string: xlabel([]) / set(h,'String',[])
        // clears the text. A restricted string-vector property still rejects "" afterwards if not
        // allowed.
        asWideStringVector.push_back(L"");
    } else {
        Error(L"Nelson:graphics:invalidDelimitedString",
            L"The value must be a '|' delimited character vector, character matrix, string array, "
            L"or cell array of character vectors.");
    }
    Dimensions dims(asWideStringVector.size(), 1);
    ArrayOf* elements
        = (ArrayOf*)ArrayOf::allocateArrayOf(NLS_CELL_ARRAY, asWideStringVector.size());
    ArrayOf res(NLS_CELL_ARRAY, dims, elements);
    for (indexType k = 0; k < dims.elementCount(); k++) {
        elements[k] = ArrayOf::characterArray(asWideStringVector[k]);
    }
    return res;
}
//=============================================================================
void
Tokenize(const std::wstring& str, std::vector<std::wstring>& tokens, const std::wstring& delimiters)
{
    std::wstring::size_type lastPos = str.find_first_not_of(delimiters, 0);
    std::wstring::size_type pos = str.find_first_of(delimiters, lastPos);

    while (std::wstring::npos != pos || std::wstring::npos != lastPos) {
        tokens.push_back(str.substr(lastPos, pos - lastPos));
        lastPos = str.find_first_not_of(delimiters, pos);
        pos = str.find_first_of(delimiters, lastPos);
    }
}
//=============================================================================
void
validateGO(int64 handle)
{
    if (handle == 0 || isUnassignedGraphicsObject(handle)) {
        return;
    }
    if (handle >= HANDLE_OFFSET_OBJECT) {
        findGraphicsObject(handle);
    } else {
        findGOFigure(handle);
    }
}
//=============================================================================
GraphicsObject*
graphicsObjectFromHandle(int64 handle, bool throwError)
{
    if (isUnassignedGraphicsObject(handle)) {
        if (throwError) {
            Error(L"Nelson:graphics:invalidHandle", L"Invalid handle.");
        }
        return nullptr;
    }
    GraphicsObject* fp = nullptr;
    if (handle == HANDLE_ROOT_OBJECT) {
        fp = getGraphicsRootObject();
        fp->updateState();
    } else if (handle >= HANDLE_OFFSET_OBJECT) {
        fp = findGraphicsObject(handle, throwError);
    } else {
        fp = static_cast<GraphicsObject*>(findGOFigure(handle));
    }
    if (!fp && throwError) {
        Error(L"Nelson:graphics:invalidHandle", L"Invalid handle.");
    }
    return fp;
}
//=============================================================================
ArrayOf
graphicsObjectHandle(int64 handle, bool throwError)
{
    GraphicsObject* object = graphicsObjectFromHandle(handle, throwError);
    if (!object) {
        return ArrayOf::graphicsObject(handle);
    }
    return ArrayOf::graphicsObject(handle, object->getClassName());
}
//=============================================================================
ArrayOf
graphicsObjectVector(const std::vector<int64>& handles)
{
    if (handles.size() == 1) {
        return graphicsObjectHandle(handles[0], false);
    }
    std::vector<go_handle> goHandles;
    goHandles.reserve(handles.size());
    std::string className;
    bool homogeneousClass = !handles.empty();
    for (size_t k = 0; k < handles.size(); ++k) {
        goHandles.push_back(static_cast<go_handle>(handles[k]));
        GraphicsObject* object = graphicsObjectFromHandle(handles[k], false);
        std::string currentClassName = object ? object->getClassName() : std::string();
        if (currentClassName.empty()) {
            homogeneousClass = false;
        } else if (k == 0) {
            className = currentClassName;
        } else if (currentClassName != className) {
            homogeneousClass = false;
        }
    }
    return ArrayOf::graphicsObject(
        goHandles, Dimensions(handles.size(), 1), homogeneousClass ? className : std::string());
}
//=============================================================================
bool
stacksChildren(GraphicsObject* object)
{
    if (!object) {
        return false;
    }
    // Only the container types below are reordered. Other objects keep the stored handle order:
    // tab groups, trees and grid layouts follow insertion order, menus are already stored in the
    // reported order, and chart children fill fixed roles that the chart reads back by position.
    return object->isType(GO_PROPERTY_VALUE_ROOT_STR)
        || object->isType(GO_PROPERTY_VALUE_FIGURE_STR)
        || object->isType(GO_PROPERTY_VALUE_AXES_STR)
        || object->isType(GO_PROPERTY_VALUE_POLAR_AXES_STR)
        || object->isType(GO_PROPERTY_VALUE_HGGROUP_STR)
        || object->isType(GO_PROPERTY_VALUE_HGTRANSFORM_STR)
        || object->isType(GO_PROPERTY_VALUE_TILED_LAYOUT_STR)
        || object->isType(GO_PROPERTY_VALUE_UIPANEL_STR)
        || object->isType(GO_PROPERTY_VALUE_UITAB_STR)
        || object->isType(GO_PROPERTY_VALUE_UIBUTTONGROUP_STR);
} //=============================================================================
//=============================================================================
static bool
isOpaqueChartHostAxis(int64 handle)
{
    GraphicsObject* axis = graphicsObjectFromHandle(handle, false);
    if (!axis || !axis->isType(GO_PROPERTY_VALUE_AXES_STR)) {
        return false;
    }
    auto* children = dynamic_cast<GOGObjectsProperty*>(
        axis->findProperty(GO_CHILDREN_PROPERTY_NAME_STR, false));
    if (!children) {
        return false;
    }
    for (int64 childHandle : children->data()) {
        GraphicsObject* child = findGraphicsObject(childHandle, false);
        if (child && child->isType(GO_PROPERTY_VALUE_HEATMAP_STR)) {
            return true;
        }
    }
    return false;
}
//=============================================================================
std::vector<int64>
reportedChildren(GraphicsObject* object)
{
    if (!object) {
        return {};
    }
    if (object->isType(GO_PROPERTY_VALUE_ROOT_STR)) {
        // Root.Children is already visibility-filtered; enumeration needs all figures.
        std::vector<int64> handles = getFigureGraphicsObjects();
        std::reverse(handles.begin(), handles.end());
        return handles;
    }
    if (object->isType(GO_PROPERTY_VALUE_SCATTER_HISTOGRAM_STR)
        || object->isType(GO_PROPERTY_VALUE_STACKED_LINE_CHART_STR)
        || object->isType(GO_PROPERTY_VALUE_BUBBLE_CLOUD_STR)
        || object->isType(GO_PROPERTY_VALUE_HEATMAP_STR)) {
        return {};
    }
    auto* children = dynamic_cast<GOGObjectsProperty*>(
        object->findProperty(GO_CHILDREN_PROPERTY_NAME_STR, false));
    if (!children) {
        return {};
    }
    std::vector<int64> handles(children->data());
    if (object->isType(GO_PROPERTY_VALUE_FIGURE_STR)) {
        handles.erase(
            std::remove_if(handles.begin(), handles.end(), isOpaqueChartHostAxis), handles.end());
    }
    if (stacksChildren(object)) {
        std::reverse(handles.begin(), handles.end());
    }
    return handles;
}
//=============================================================================
bool
isVisibleGraphicsHandle(int64 handle)
{
    GraphicsObject* fp = graphicsObjectFromHandle(handle, false);
    if (!fp) {
        return false;
    }
    GOGenericProperty* hp = fp->findProperty(GO_HANDLE_VISIBILITY_PROPERTY_NAME_STR, false);
    if (!hp) {
        return true;
    }
    return StringHelpers::iequals(hp->get().asWideString(), GO_PROPERTY_VALUE_ON_STR);
}
//=============================================================================
void
setCurrentObject(int64 handle)
{
    if (isUnassignedGraphicsObject(handle) || isDeletedGraphicsObject(handle)) {
        return;
    }
    GraphicsObject* fp = graphicsObjectFromHandle(handle, false);
    GOFigure* fig = nullptr;
    if (!fp) {
        return;
    }
    if (fp->isType(GO_PROPERTY_VALUE_FIGURE_STR)) {
        fig = static_cast<GOFigure*>(fp);
    } else {
        fig = fp->getParentFigure();
        if (!fig) {
            fig = getCurrentGOFigure();
        }
    }
    if (fig) {
        fig->setGoProperty(GO_CURRENT_OBJECT_PROPERTY_NAME_STR, handle);
    }
}
//=============================================================================
bool
deleteGraphicsObject(int64 handle, bool repaintParentFigure, bool removeRefInParent)
{
    GraphicsObject* gp = findGraphicsObject(handle, false);
    if (!gp) {
        return false;
    }

    GOOnOffProperty* goOnOff
        = (GOOnOffProperty*)gp->findProperty(GO_BEING_DELETED_PROPERTY_NAME_STR, false);
    if (goOnOff) {
        goOnOff->data(GO_PROPERTY_VALUE_ON_STR);
    }
    GOCallbackProperty* goCallback
        = (GOCallbackProperty*)gp->findProperty(GO_DELETE_FCN_PROPERTY_NAME_STR, false);
    if (goCallback) {
        goCallback->executeNow(gp, L"EventData", L"ObjectBeingDestroyed");
    }

    GOGObjectsProperty* hp = (GOGObjectsProperty*)gp->findProperty(GO_CHILDREN_PROPERTY_NAME_STR);
    if (hp) {
        std::vector<int64> children(hp->data());
        if (!children.empty()) {
            for (auto c : children) {
                deleteGraphicsObject(c, false, true);
            }
            children.clear();
            hp->data(children);
        }
    }

    if (removeRefInParent) {
        GOGObjectsProperty* gop
            = (GOGObjectsProperty*)gp->findProperty(GO_PARENT_PROPERTY_NAME_STR);
        if (gop) {
            std::vector<int64> parentHandles(gop->data());
            if (!parentHandles.empty()) {
                for (auto h : parentHandles) {
                    GraphicsObject* gh = nullptr;
                    if (h >= HANDLE_OFFSET_OBJECT) {
                        gh = findGraphicsObject(h, false);
                    } else if (h != HANDLE_ROOT_OBJECT && h != 0) {
                        gh = static_cast<GraphicsObject*>(findGOFigure(h));
                    }
                    if (gh) {
                        hp = (GOGObjectsProperty*)gh->findProperty(GO_CHILDREN_PROPERTY_NAME_STR);
                        if (hp) {
                            std::vector<int64> childrenParent(hp->data());
                            std::vector<int64> filtered;
                            filtered.reserve(childrenParent.size());
                            for (auto c : childrenParent) {
                                if (c != handle) {
                                    filtered.push_back(c);
                                }
                            }
                            hp->data(filtered);
                        }
                    }
                }
            }
        }
    }
    clearAxesToolbarReference(gp, handle);
    restoreColorBarPeerPosition(gp);
    restoreLegendPeerPosition(gp);
    clearLegendPeerReference(gp);
    clearParentInternalObjectReference(gp, handle);
    if (gp->isType(GO_PROPERTY_VALUE_AXES_STR) || gp->isType(GO_PROPERTY_VALUE_POLAR_AXES_STR)) {
        deleteColorBarsForPeer(handle);
        deleteLegendsForPeer(handle);
    }
    clearFigureCurrentReferences(handle);
    freeGraphicsObject(handle);
    if (repaintParentFigure) {
        GOFigure* parentFigure = nullptr;
        if (!gp->isType(GO_PROPERTY_VALUE_FIGURE_STR) && !gp->isType(GO_PROPERTY_VALUE_ROOT_STR)) {
            parentFigure = gp->getParentFigure();
        }
        if (parentFigure) {
            if (!parentFigure->isDrawLater()) {
                parentFigure->repaint();
            }
        }
    }
    delete gp;
    return true;
}
//=============================================================================
GraphicsObject*
findGraphicsObject(int64 handle, bool throwError)
{
    return objectset.findGO(handle - HANDLE_OFFSET_OBJECT, throwError);
}
//=============================================================================
GOFigure*
findGOFigure(int64 handle)
{
    int64 id = handle;
    checkIdValidity(id);
    GOFigureHost* window = getFigure(id);
    if (window) {
        return window->hostFigure();
    }
    return nullptr;
}
//=============================================================================
GOFigureHost*
findGOWindows(int64 handle)
{
    int64 id = handle;
    checkIdValidity(id);
    return getFigure(id);
}
//=============================================================================
int64
assignGraphicsObject(GraphicsObject* hp)
{
    return (objectset.assignGO(hp) + HANDLE_OFFSET_OBJECT);
}
//=============================================================================
int64
findGraphicsObjectHandle(GraphicsObject* hp)
{
    if (!hp) {
        return -1;
    }
    int64 handle = objectset.findHandle(hp);
    if (handle != -1) {
        return handle + HANDLE_OFFSET_OBJECT;
    }
    std::map<int64, GOFigureHost*> figures = getFigureList();
    for (const auto& it : figures) {
        if (it.second && it.second->hostFigure() == hp) {
            return it.first;
        }
    }
    return -1;
}
//=============================================================================
// A character matrix carries one entry per row, the way cellstr reads it. A row
// vector is not one: it keeps the '|' delimited reading.
bool
isCharacterMatrix(const ArrayOf& arg)
{
    if (!arg.isCharacterArray() || arg.isEmpty()) {
        return false;
    }
    Dimensions dims = arg.dimensions();
    return dims.is2D() && dims.rows() > 1;
}
//=============================================================================
wstringVector
characterMatrixRows(const ArrayOf& arg)
{
    Dimensions dims = arg.dimensions();
    const indexType rows = dims.rows();
    const indexType columns = dims.columns();
    const charType* ptr = static_cast<const charType*>(arg.dataPointer());
    wstringVector result;
    result.reserve(rows);
    for (indexType i = 0; i < rows; i++) {
        std::wstring line;
        line.reserve(columns);
        for (indexType j = 0; j < columns; j++) {
            line.push_back(ptr[i + j * rows]);
        }
        // The shorter rows of a character matrix are padded with blanks; cellstr
        // drops that padding and a text label must not render it either.
        while (!line.empty() && (line.back() == 0 || iswspace(line.back()))) {
            line.pop_back();
        }
        result.push_back(line);
    }
    return result;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
