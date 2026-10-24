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
#include <vector>
#include "copyobjBuiltin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "NelsonException.hpp"
#include "GOAnimatedLine.hpp"
#include "GOAnnotation.hpp"
#include "GOAxis.hpp"
#include "GOColorBar.hpp"
#include "GOFiguresManager.hpp"
#include "GOGObjectsProperty.hpp"
#include "GOHelpers.hpp"
#include "GOLegend.hpp"
#include "GOMenuBuilder.hpp"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "GraphicsObjectFactory.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
namespace Nelson::GraphicsGateway {
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
static int64
defaultParentFor(GraphicsObject* source)
{
    GOGObjectsProperty* parent = static_cast<GOGObjectsProperty*>(
        source->findProperty(GO_PARENT_PROPERTY_NAME_STR, false));
    if (parent && !parent->data().empty()) {
        return parent->data()[0];
    }
    return HANDLE_ROOT_OBJECT;
}
//=============================================================================
static void
copyProperties(GraphicsObject* source, GraphicsObject* target)
{
    for (const auto& name : source->fieldnames()) {
        if (!canCopyGraphicsProperty(source, target, name)) {
            continue;
        }
        try {
            target->findProperty(name)->set(source->findProperty(name)->get());
        } catch (const NelsonException&) {
        }
    }
}
//=============================================================================
static void
copyAxisInternalState(GraphicsObject* source, GraphicsObject* target)
{
    if (!source->isType(GO_PROPERTY_VALUE_AXES_STR)
        || !target->isType(GO_PROPERTY_VALUE_AXES_STR)) {
        return;
    }
    GOGenericProperty* sourcePolar = source->findProperty(GO_POLAR_AXES_PROPERTY_NAME_STR, false);
    GOGenericProperty* targetPolar = target->findProperty(GO_POLAR_AXES_PROPERTY_NAME_STR, false);
    if (sourcePolar && targetPolar) {
        targetPolar->set(sourcePolar->get());
    }
}
//=============================================================================
static void
copyColorBarInternalState(GraphicsObject* source, GraphicsObject* target, int64 targetHandle)
{
    GOColorBar* sourceColorBar = dynamic_cast<GOColorBar*>(source);
    GOColorBar* targetColorBar = dynamic_cast<GOColorBar*>(target);
    if (!sourceColorBar || !targetColorBar) {
        return;
    }
    targetColorBar->setPeerAxesHandle(sourceColorBar->getPeerAxesHandle());
    std::wstring location = targetColorBar->findStringProperty(GO_LOCATION_PROPERTY_NAME_STR);
    targetColorBar->createLayoutOptions(targetHandle, location == L"layout" ? L"east" : L"");
    int64 targetLabelHandle = targetColorBar->createLabel(targetHandle);
    int64 sourceLabelHandle = sourceColorBar->getLabelHandle();
    GraphicsObject* sourceLabel = findGraphicsObject(sourceLabelHandle, false);
    GraphicsObject* targetLabel = findGraphicsObject(targetLabelHandle, false);
    if (sourceLabel && targetLabel) {
        copyProperties(sourceLabel, targetLabel);
        targetLabel->setGoProperty(GO_PARENT_PROPERTY_NAME_STR, targetHandle);
        targetLabel->updateState();
    }
}
//=============================================================================
static void
copyLegendInternalState(GraphicsObject* source, GraphicsObject* target, int64 targetHandle)
{
    GOLegend* sourceLegend = dynamic_cast<GOLegend*>(source);
    GOLegend* targetLegend = dynamic_cast<GOLegend*>(target);
    if (!sourceLegend || !targetLegend) {
        return;
    }
    targetLegend->setPeerAxesHandle(sourceLegend->getPeerAxesHandle());
    targetLegend->setPlotHandles(sourceLegend->getPlotHandles());
    targetLegend->createLayoutOptions(targetHandle, L"");
    int64 targetTitleHandle = targetLegend->createTitle(targetHandle);
    int64 sourceTitleHandle = sourceLegend->getTitleHandle();
    GraphicsObject* sourceTitle = findGraphicsObject(sourceTitleHandle, false);
    GraphicsObject* targetTitle = findGraphicsObject(targetTitleHandle, false);
    if (sourceTitle && targetTitle) {
        copyProperties(sourceTitle, targetTitle);
        targetTitle->setGoProperty(GO_PARENT_PROPERTY_NAME_STR, targetHandle);
        targetTitle->updateState();
    }
}
//=============================================================================
static void
copyAnimatedLineInternalState(GraphicsObject* source, GraphicsObject* target)
{
    GOAnimatedLine* sourceLine = dynamic_cast<GOAnimatedLine*>(source);
    GOAnimatedLine* targetLine = dynamic_cast<GOAnimatedLine*>(target);
    if (!sourceLine || !targetLine) {
        return;
    }
    targetLine->setPoints(sourceLine->xData(), sourceLine->yData(), sourceLine->zData());
}
//=============================================================================
static void
copyAnnotationInternalState(GraphicsObject* source, GraphicsObject* target)
{
    if (!dynamic_cast<GOAnnotation*>(source) || !dynamic_cast<GOAnnotation*>(target)) {
        return;
    }
    try {
        target->findProperty(GO_UNITS_PROPERTY_NAME_STR)
            ->set(source->findProperty(GO_UNITS_PROPERTY_NAME_STR)->get());
        target->updateState();
        target->findProperty(GO_POSITION_PROPERTY_NAME_STR)
            ->set(source->findProperty(GO_POSITION_PROPERTY_NAME_STR)->get());
        if (source->haveProperty(GO_X_PROPERTY_NAME_STR)
            && target->haveProperty(GO_X_PROPERTY_NAME_STR)) {
            target->findProperty(GO_X_PROPERTY_NAME_STR)
                ->set(source->findProperty(GO_X_PROPERTY_NAME_STR)->get());
            target->findProperty(GO_Y_PROPERTY_NAME_STR)
                ->set(source->findProperty(GO_Y_PROPERTY_NAME_STR)->get());
        }
        target->updateState();
    } catch (const NelsonException&) {
    }
}
//=============================================================================
static int64
copyOne(int64 sourceHandle, int64 parentHandle)
{
    GraphicsObject* source = graphicsObjectFromHandle(sourceHandle, true);
    if (source->isType(GO_PROPERTY_VALUE_ROOT_STR)) {
        Error(L"Nelson:copyobj:cannotCopyRoot", L"Cannot copy root graphics object.");
    }

    int64 newHandle = HANDLE_UNASSIGNED_OBJECT;
    GraphicsObject* target = nullptr;
    if (source->isType(GO_PROPERTY_VALUE_FIGURE_STR)) {
        newHandle = createNewFigure(false);
        target = static_cast<GraphicsObject*>(findGOFigure(newHandle));
    } else {
        target = newGraphicsObjectForSource(source);
        if (!target) {
            Error(L"Nelson:graphics:unsupportedType", L"Unsupported graphics object type.");
        }
        newHandle = assignGraphicsObject(target);
        int64 resolvedParent
            = parentHandle == HANDLE_UNASSIGNED_OBJECT ? defaultParentFor(source) : parentHandle;
        if (target->isType(GO_PROPERTY_VALUE_UICONTEXTMENU_STR)
            || target->isType(GO_PROPERTY_VALUE_UIMENU_STR)) {
            setGraphicsMenuParent(newHandle, resolvedParent);
        } else {
            target->setGoProperty(GO_PARENT_PROPERTY_NAME_STR, resolvedParent);
            appendChild(resolvedParent, newHandle);
        }
        if (target->isType(GO_PROPERTY_VALUE_AXES_STR)) {
            static_cast<GOAxis*>(target)->restoreToolbar(newHandle);
        }
    }

    copyProperties(source, target);
    copyAxisInternalState(source, target);
    copyColorBarInternalState(source, target, newHandle);
    copyLegendInternalState(source, target, newHandle);
    copyAnimatedLineInternalState(source, target);
    copyAnnotationInternalState(source, target);

    GOGObjectsProperty* children = static_cast<GOGObjectsProperty*>(
        source->findProperty(GO_CHILDREN_PROPERTY_NAME_STR, false));
    if (children) {
        for (int64 child : children->data()) {
            if (!isDeletedGraphicsObject(child)) {
                copyOne(child, newHandle);
            }
        }
    }

    target->updateState();
    return newHandle;
}
//=============================================================================
ArrayOfVector
copyobjBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 2);
    nargoutcheck(nLhs, 0, 1);
    if (!argIn[0].isGraphicsObject()) {
        Error(L"Nelson:graphics:expectedGraphicsObject:expectedGraphicsObjectS",
            L"Expected graphics object(s).");
    }

    int64 parentHandle = HANDLE_UNASSIGNED_OBJECT;
    if (argIn.size() == 2) {
        parentHandle = argIn[1].asGraphicsObjectScalar();
        graphicsObjectFromHandle(parentHandle, true);
    }

    Dimensions dims = argIn[0].dimensions();
    nelson_handle* copied = static_cast<nelson_handle*>(
        ArrayOf::allocateArrayOf(NLS_GO_HANDLE, dims.elementCount(), stringVector(), false));
    const nelson_handle* source = static_cast<const nelson_handle*>(argIn[0].dataPointer());
    // Each copy is appended to the destination, so the one copied last ends up on
    // top. Walking a stacking destination backwards therefore leaves it reporting
    // the copies in the order the sources were handed in.
    bool backwards = false;
    if (dims.elementCount() > 1) {
        int64 destination = parentHandle;
        if (destination == HANDLE_UNASSIGNED_OBJECT) {
            destination = defaultParentFor(graphicsObjectFromHandle(source[0], true));
        }
        backwards = stacksChildren(graphicsObjectFromHandle(destination, false));
    }
    for (indexType k = 0; k < dims.elementCount(); ++k) {
        indexType index = backwards ? dims.elementCount() - 1 - k : k;
        copied[index] = copyOne(source[index], parentHandle);
    }

    ArrayOfVector retval;
    retval << ArrayOf(NLS_GO_HANDLE, dims, copied);
    return retval;
}
//=============================================================================
} // namespace Nelson::GraphicsGateway
//=============================================================================
