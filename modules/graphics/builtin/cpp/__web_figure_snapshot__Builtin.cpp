//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "__web_figure_snapshot__Builtin.hpp"
#include "GOAxis.hpp"
#include "GOFigure.hpp"
#include "GOHelpers.hpp"
#include "FindObj.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include <cmath>
//=============================================================================
namespace Nelson::GraphicsGateway {
//=============================================================================
namespace {
    ArrayOf
    cells(const ArrayOfVector& values)
    {
        if (values.empty()) {
            return ArrayOf::emptyCell(Dimensions(0, 0));
        }
        ArrayOfMatrix rows;
        rows.push_back(values);
        return ArrayOf::cellArray(rows);
    }
    //=============================================================================
    GOFigure*
    figureArgument(const ArrayOf& value)
    {
        auto* figure = dynamic_cast<GOFigure*>(
            graphicsObjectFromHandle(value.asGraphicsObjectScalar(), false));
        if (!figure) {
            Error(L"Nelson:graphics:expectedFigure", L"Figure object expected.");
        }
        return figure;
    }
    //=============================================================================
    ArrayOf
    objectsOfAxes(const ArrayOf& axis, bool& floor)
    {
        const auto children = FindObj({ axis }, true);
        const auto* handles = static_cast<const nelson_handle*>(children.dataPointer());
        ArrayOfVector objects;
        for (indexType index = 0; index < children.elementCount(); ++index) {
            auto* object = findGraphicsObject(handles[index], false);
            if (!object) {
                continue;
            }
            auto* tag = object->findProperty(L"Tag", false);
            const bool projection = tag && tag->get().asWideString() == L"__web_floor_projection__";
            floor = floor || projection;
            const auto type = object->getType();
            if (projection
                || (type != L"line" && type != L"scatter" && type != L"surface"
                    && type != L"patch")) {
                continue;
            }
            wstringVector names { L"handle", L"type", L"visible" };
            ArrayOfVector values;
            values << ArrayOf::doubleScalar(static_cast<double>(handles[index]))
                   << ArrayOf::characterArray(type) << object->findProperty(L"Visible")->get();
            for (const auto& name :
                { L"DisplayName", L"LineWidth", L"Color", L"FaceColor", L"EdgeColor", L"LineStyle",
                    L"Marker", L"MarkerSize", L"MarkerFaceColor", L"MarkerEdgeColor" }) {
                if (auto* property = object->findProperty(name, false)) {
                    names.push_back(name);
                    values.push_back(property->get());
                }
            }
            objects.push_back(ArrayOf::structScalar(names, values));
        }
        return cells(objects);
    }
    //=============================================================================
    ArrayOf
    axesSnapshot(const ArrayOf& axis)
    {
        auto* object
            = dynamic_cast<GOAxis*>(findGraphicsObject(axis.asGraphicsObjectScalar(), false));
        object->updateStateIfDirty();
        bool floor = false;
        const auto objects = objectsOfAxes(axis, floor);
        auto* title = findGraphicsObject(object->findGoProperty(L"Title"), false);
        wstringVector names { L"handle", L"title", L"objects", L"projectionFloor" };
        ArrayOfVector values;
        values << ArrayOf::doubleScalar(static_cast<double>(axis.asGraphicsObjectScalar()))
               << title->findProperty(L"String")->get() << objects << ArrayOf::logicalScalar(floor);
        for (const auto& name : { L"View", L"XLim", L"YLim", L"ZLim", L"XLimMode", L"YLimMode",
                 L"ZLimMode", L"XGrid", L"YGrid", L"ZGrid", L"XScale", L"YScale", L"ZScale",
                 L"XDir", L"YDir", L"ZDir", L"CameraPosition", L"CameraTarget", L"CameraUpVector",
                 L"CameraViewAngle", L"CameraPositionMode", L"CameraTargetMode",
                 L"CameraUpVectorMode", L"CameraViewAngleMode", L"Projection" }) {
            names.push_back(name);
            values.push_back(object->findProperty(name)->get());
        }
        return ArrayOf::structScalar(names, values);
    }
    //=============================================================================
} // namespace
//=============================================================================
ArrayOfVector
__web_figure_snapshot__Builtin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 1);
    auto* figure = figureArgument(argIn[0]);
    ArrayOfVector query;
    query << argIn[0] << ArrayOf::characterArray(L"Type") << ArrayOf::characterArray(L"axes");
    const auto axes = FindObj(query, true);
    const auto* handles = static_cast<const nelson_handle*>(axes.dataPointer());
    ArrayOfVector entries;
    for (indexType index = 0; index < axes.elementCount(); ++index) {
        entries.push_back(axesSnapshot(graphicsObjectHandle(handles[index])));
    }
    ArrayOfVector values;
    values << ArrayOf::doubleScalar(static_cast<double>(argIn[0].asGraphicsObjectScalar()))
           << figure->findProperty(L"Name")->get() << figure->findProperty(L"NumberTitle")->get()
           << figure->findProperty(L"Color")->get()
           << ArrayOf::doubleScalar(static_cast<double>(figure->findGoProperty(L"CurrentAxes")))
           << cells(entries);
    const auto history = figure->workspaceHistory();
    values << ArrayOf::logicalScalar(
        history.isStruct() && history.isField("undo") && !history.field("undo").isEmpty())
           << ArrayOf::logicalScalar(history.isStruct() && history.isField("redo")
                  && !history.field("redo").isEmpty());
    return { ArrayOf::structScalar({ L"handle", L"name", L"numberTitle", L"color", L"currentAxes",
                                       L"axes", L"canUndo", L"canRedo" },
        values) };
}
//=============================================================================
ArrayOfVector
__web_figure_state__Builtin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 1);
    const double handle = argIn[0].asDoubleScalar();
    if (!std::isfinite(handle) || handle < 0 || handle >= HANDLE_OFFSET_OBJECT
        || std::floor(handle) != handle) {
        Error(L"Nelson:graphics:expectedFigure", L"Figure object expected.");
    }
    ArrayOfVector inputs;
    inputs << graphicsObjectHandle(static_cast<int64>(handle));
    return __web_figure_snapshot__Builtin(nLhs, inputs);
}
//=============================================================================
ArrayOfVector
__web_figure_history_data__Builtin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 2);
    nargoutcheck(nLhs, 0, 1);
    auto* figure = figureArgument(argIn[0]);
    if (argIn.size() == 2) {
        figure->workspaceHistory() = argIn[1];
    }
    return { figure->workspaceHistory() };
}
//=============================================================================
} // namespace Nelson::GraphicsGateway
//=============================================================================
