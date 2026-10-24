//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "__wordcloudchart__Builtin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "NelsonException.hpp"
#include "GOFiguresManager.hpp"
#include "GOGObjectsProperty.hpp"
#include "GOWordCloudChart.hpp"
#include "GOHelpers.hpp"
#include "GOPropertyNames.hpp"
#include "RefreshFigure.hpp"
#include "i18n.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
namespace Nelson::GraphicsGateway {
//=============================================================================
static int64
parentFigureHandle()
{
    getCurrentGOFigure();
    return getCurrentFigure();
}
//=============================================================================
static void
appendFigureChild(GOFigure* fig, int64 childHandle)
{
    GOGObjectsProperty* children
        = static_cast<GOGObjectsProperty*>(fig->findProperty(GO_CHILDREN_PROPERTY_NAME_STR));
    std::vector<int64> handles(children->data());
    handles.push_back(childHandle);
    children->data(handles);
}
//=============================================================================
static int64
applyProperties(GraphicsObject* object, const ArrayOfVector& argIn)
{
    if (argIn.size() % 2 != 0) {
        Error(L"Nelson:graphics:nameValuePair", L"Name-value arguments must be paired.");
    }
    int64 parentHandle = parentFigureHandle();
    for (indexType k = 0; k < argIn.size(); k += 2) {
        if (!argIn[k].isRowVectorCharacterArray() && !argIn[k].isScalarStringArray()) {
            Error(L"Nelson:graphics:propertyNameNotText",
                L"Property name must be a string scalar or character row vector.");
        }
        std::wstring propname = argIn[k].asWideString();
        ArrayOf value = argIn[k + 1];
        if (propname == GO_AUTO_PARENT_PROPERTY_NAME_STR) {
            continue;
        }
        if (!object->isWritable(propname) && object->haveProperty(propname)) {
            Error(L"Nelson:wordcloud:readOnlyProperty", L"Property is read-only: %s", propname);
        }
        try {
            if (propname == GO_PARENT_PROPERTY_NAME_STR) {
                parentHandle = value.asGraphicsObjectScalar();
            }
            object->findProperty(propname)->set(value);
        } catch (const NelsonException& e) {
            RaiseError(formatI18nMessage(
                           Nelson::resolveMessageTemplate(
                               L"Nelson:graphics:gotErrorProperty", L"Got error for property: %s"),
                           propname)
                    + L"\n" + e.what(),
                L"Nelson:wordcloud:propertyError");
        }
    }
    return parentHandle;
}
//=============================================================================
ArrayOfVector
__wordcloudchart__Builtin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    GraphicsObject* chart = new GOWordCloudChart;
    int64 chartHandle = assignGraphicsObject(chart);
    int64 figHandle = applyProperties(chart, argIn);
    GOFigure* fig = findGOFigure(figHandle);
    if (!fig) {
        figHandle = parentFigureHandle();
        fig = findGOFigure(figHandle);
        chart->setGoProperty(GO_PARENT_PROPERTY_NAME_STR, figHandle);
    }
    if (fig) {
        if (chart->findGoProperty(GO_PARENT_PROPERTY_NAME_STR) == 0) {
            chart->setGoProperty(GO_PARENT_PROPERTY_NAME_STR, figHandle);
        }
        appendFigureChild(fig, chartHandle);
    }
    chart->updateState();
    if (fig) {
        fig->setRenderingStateInvalid(true);
        if (!fig->isDrawLater()) {
            refreshFigure(fig);
        }
    }
    retval << graphicsObjectHandle(chartHandle);
    return retval;
}
//=============================================================================
} // namespace Nelson::GraphicsGateway
//=============================================================================
