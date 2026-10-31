//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <algorithm>
#include <cmath>
#include <limits>
#include "GOBubbleCloud.hpp"
#include "GOChartDecorators.hpp"
#include "GOArrayOfProperty.hpp"
#include "GOAutoFlatColorProperty.hpp"
#include "GOColorProperty.hpp"
#include "GOFigure.hpp"
#include "GOGObjectsProperty.hpp"
#include "GOHelpers.hpp"
#include "GOLegend.hpp"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "GOStringProperty.hpp"
#include "GOUnitsProperty.hpp"
#include "GOVectorFourDoubleProperty.hpp"
#include "GOVectorProperty.hpp"
#include "GOPositionHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static int
handleIndex(const std::vector<int64>& handles, int64 handle)
{
    for (int k = 0; k < handles.size(); ++k) {
        if (handles[k] == handle) {
            return k;
        }
    }
    return -1;
}
//=============================================================================
static void
resizePixelLegendToFitTitle(GOLegend* legend)
{
    if (!legend || !legend->stringCheck(GO_UNITS_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_PIXELS_STR)) {
        return;
    }
    std::vector<double> position = legend->findVectorDoubleProperty(GO_POSITION_PROPERTY_NAME_STR);
    if (position.size() < 4) {
        return;
    }
    auto preferredSize = legend->preferredSizeInPixels();
    double width = std::max(position[2], std::ceil(preferredSize.first));
    double height = std::max(position[3], std::ceil(preferredSize.second));
    double right = position[0] + position[2];
    double top = position[1] + position[3];
    legend->setFourVectorDefault(
        GO_POSITION_PROPERTY_NAME_STR, right - width, top - height, width, height);
    legend->clearChanged(GO_POSITION_PROPERTY_NAME_STR);
}
//=============================================================================
GOBubbleCloud::GOBubbleCloud() : GOGroup()
{
    registerProperty(new GOVectorProperty, GO_SIZE_DATA_PROPERTY_NAME_STR);
    registerProperty(new GOArrayOfProperty, GO_LABEL_DATA_PROPERTY_NAME_STR);
    registerProperty(new GOArrayOfProperty, GO_GROUP_DATA_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_TITLE_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_LEGEND_TITLE_PROPERTY_NAME_STR);
    registerProperty(new GOAutoFlatColorProperty, GO_FACE_COLOR_PROPERTY_NAME_STR);
    registerProperty(new GOColorProperty, GO_EDGE_COLOR_PROPERTY_NAME_STR);
    registerProperty(new GOUnitsProperty, GO_UNITS_PROPERTY_NAME_STR);
    registerProperty(new GOFourVectorProperty, GO_OUTER_POSITION_PROPERTY_NAME_STR);
    registerProperty(new GOFourVectorProperty, GO_INNER_POSITION_PROPERTY_NAME_STR);
    registerProperty(new GOFourVectorProperty, GO_POSITION_PROPERTY_NAME_STR);
    registerProperty(new GOSourceTableProperty, GO_SOURCE_TABLE_PROPERTY_NAME_STR);
    registerProperty(new GOArrayOfProperty, GO_SIZE_VARIABLE_PROPERTY_NAME_STR);
    registerProperty(new GOArrayOfProperty, GO_LABEL_VARIABLE_PROPERTY_NAME_STR);
    registerProperty(new GOArrayOfProperty, GO_GROUP_VARIABLE_PROPERTY_NAME_STR);
    sortProperties();

    setStringDefault(GO_TYPE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_BUBBLE_CLOUD_STR);
    setVectorDoubleDefault(GO_SIZE_DATA_PROPERTY_NAME_STR, {});
    findProperty(GO_LABEL_DATA_PROPERTY_NAME_STR)->set(ArrayOf::empty());
    findProperty(GO_GROUP_DATA_PROPERTY_NAME_STR)->set(ArrayOf::empty());
    setStringDefault(GO_TITLE_PROPERTY_NAME_STR, L"");
    setStringDefault(GO_LEGEND_TITLE_PROPERTY_NAME_STR, L"");
    setRestrictedStringColorDefault(
        GO_FACE_COLOR_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_FLAT_STR, 0, 0, 0);
    setThreeVectorDefault(
        GO_EDGE_COLOR_PROPERTY_NAME_STR, 0.129411764705882, 0.129411764705882, 0.129411764705882);
    setRestrictedStringDefault(GO_UNITS_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_NORMALIZED_STR);
    setFourVectorDefault(GO_OUTER_POSITION_PROPERTY_NAME_STR, 0, 0, 1, 1);
    setFourVectorDefault(GO_INNER_POSITION_PROPERTY_NAME_STR, 0.13, 0.11, 0.775, 0.815);
    setFourVectorDefault(GO_POSITION_PROPERTY_NAME_STR, 0.13, 0.11, 0.775, 0.815);
    findProperty(GO_SOURCE_TABLE_PROPERTY_NAME_STR)->set(ArrayOf::empty());
    findProperty(GO_SIZE_VARIABLE_PROPERTY_NAME_STR)->set(ArrayOf::empty());
    findProperty(GO_LABEL_VARIABLE_PROPERTY_NAME_STR)->set(ArrayOf::empty());
    findProperty(GO_GROUP_VARIABLE_PROPERTY_NAME_STR)->set(ArrayOf::empty());
    clearAllChanged();
}
//=============================================================================
std::wstring
GOBubbleCloud::getType()
{
    return GO_PROPERTY_VALUE_BUBBLE_CLOUD_STR;
}
//=============================================================================
void
GOBubbleCloud::updateState()
{
    syncChartPositionUnits(this);
    GOGroup::updateState();
    syncLegendTitle();
}
//=============================================================================
void
GOBubbleCloud::syncLegendTitle()
{
    GOFigure* fig = getParentFigure();
    if (!fig) {
        return;
    }
    int64 cloudHandle = findGraphicsObjectHandle(this);
    GOGObjectsProperty* childrenProperty
        = static_cast<GOGObjectsProperty*>(fig->findProperty(GO_CHILDREN_PROPERTY_NAME_STR, false));
    if (!childrenProperty) {
        return;
    }
    std::vector<int64> children = childrenProperty->data();
    GOGObjectsProperty* chartChildrenProperty
        = static_cast<GOGObjectsProperty*>(findProperty(GO_CHILDREN_PROPERTY_NAME_STR, false));
    std::vector<int64> chartChildren
        = chartChildrenProperty ? chartChildrenProperty->data() : std::vector<int64>();
    int cloudIndex = handleIndex(children, cloudHandle);
    if (cloudIndex < 0) {
        return;
    }
    std::wstring legendTitle = findStringProperty(GO_LEGEND_TITLE_PROPERTY_NAME_STR);
    GOLegend* selectedLegend = nullptr;
    int64 selectedLegendHandle = 0;
    int selectedDistance = std::numeric_limits<int>::max();
    for (auto handle : children) {
        GraphicsObject* object = findGraphicsObject(handle, false);
        auto* legend = dynamic_cast<GOLegend*>(object);
        if (!legend) {
            continue;
        }
        int peerIndex = handleIndex(children, legend->getPeerAxesHandle());
        int distance = 0;
        if (peerIndex < 0) {
            if (handleIndex(chartChildren, legend->getPeerAxesHandle()) < 0) {
                continue;
            }
        } else {
            distance = std::abs(peerIndex - cloudIndex);
        }
        if (distance < selectedDistance) {
            selectedDistance = distance;
            selectedLegend = legend;
            selectedLegendHandle = handle;
        }
    }
    if (!selectedLegend) {
        return;
    }
    int64 titleHandle = selectedLegend->findGoProperty(GO_TITLE_PROPERTY_NAME_STR);
    if (titleHandle < HANDLE_OFFSET_OBJECT) {
        titleHandle = selectedLegend->createTitle(selectedLegendHandle);
    }
    GraphicsObject* title = findGraphicsObject(titleHandle, false);
    if (!title) {
        return;
    }
    title->findProperty(GO_STRING_PROPERTY_NAME_STR)->set(ArrayOf::characterArray(legendTitle));
    title->updateState();
    selectedLegend->updateState();
    resizePixelLegendToFitTitle(selectedLegend);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
