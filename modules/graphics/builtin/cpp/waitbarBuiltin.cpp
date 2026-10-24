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
#include "waitbarBuiltin.hpp"
#include "__patch__Builtin.hpp"
#include "axesBuiltin.hpp"
#include "dialogBuiltin.hpp"
#include "findobjBuiltin.hpp"
#include "graphics_object_setBuiltin.hpp"
#include "GOFigure.hpp"
#include "GOHelpers.hpp"
#include "GOPropertyNames.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
namespace Nelson::GraphicsGateway {
//=============================================================================
static void
setObjectProperty(const ArrayOf& handle, const std::wstring& property, const ArrayOf& value);
static ArrayOf
createWaitBarFigure();
static ArrayOf
createWaitBarAxes(const ArrayOf& figure);
static void
updateWaitBarAxes(const ArrayOf& axes, double value);
static ArrayOf
findWaitBarAxes(const ArrayOf& figure);
static void
clearCurrentObject(const ArrayOf& figure);
static void
setReferenceWaitBarGeometry(const ArrayOf& figure);
//=============================================================================
ArrayOfVector
waitbarBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 1);
    double value = std::max(0.0, std::min(1.0, argIn[0].asDoubleScalar()));
    ArrayOf handle;
    bool created = false;
    if (argIn.size() >= 2 && argIn[1].isGraphicsObject()) {
        handle = argIn[1];
        if (argIn.size() >= 3) {
            setObjectProperty(handle, L"Name", argIn[2]);
        }
    } else {
        handle = createWaitBarFigure();
        ArrayOf axes = createWaitBarAxes(handle);
        updateWaitBarAxes(axes, value);
        created = true;
    }
    ArrayOf axes = findWaitBarAxes(handle);
    if (!axes.isEmpty()) {
        updateWaitBarAxes(axes, value);
    }
    clearCurrentObject(handle);
    if (created) {
        setReferenceWaitBarGeometry(handle);
    }
    retval << handle;
    return retval;
}
//=============================================================================
static void
setObjectProperty(const ArrayOf& handle, const std::wstring& property, const ArrayOf& value)
{
    ArrayOfVector args;
    args << handle;
    args << ArrayOf::characterArray(property);
    args << value;
    graphics_object_setBuiltin(0, args);
}
//=============================================================================
static ArrayOf
createWaitBarFigure()
{
    ArrayOfVector args;
    args << ArrayOf::characterArray(L"Name");
    args << ArrayOf::characterArray(L"");
    args << ArrayOf::characterArray(L"Tag");
    args << ArrayOf::characterArray(L"WindowStyle");
    args << ArrayOf::characterArray(L"normal");
    args << ArrayOf::characterArray(L"Interruptible");
    args << ArrayOf::characterArray(L"off");
    args << ArrayOf::characterArray(L"ButtonDownFcn");
    args << ArrayOf::characterArray(L"");
    args << ArrayOf::characterArray(L"Units");
    args << ArrayOf::characterArray(L"points");
    args << ArrayOf::characterArray(L"Position");
    args << ArrayOf::doubleRowVector({ 441, 297.63, 270, 55.65 });
    ArrayOfVector result = dialogBuiltin(1, args);
    return result.empty() ? ArrayOf::empty() : result[0];
}
//=============================================================================
static ArrayOf
createWaitBarAxes(const ArrayOf& figure)
{
    ArrayOfVector args;
    args << figure;
    args << ArrayOf::characterArray(L"Visible");
    args << ArrayOf::characterArray(L"off");
    args << ArrayOf::characterArray(L"XLim");
    args << ArrayOf::doubleRowVector({ 0, 1 });
    args << ArrayOf::characterArray(L"YLim");
    args << ArrayOf::doubleRowVector({ 0, 1 });
    args << ArrayOf::characterArray(L"XTick");
    args << ArrayOf::doubleRowVector({});
    args << ArrayOf::characterArray(L"YTick");
    args << ArrayOf::doubleRowVector({});
    args << ArrayOf::characterArray(L"Box");
    args << ArrayOf::characterArray(L"on");
    ArrayOfVector result = axesBuiltin(1, args);
    if (result.empty()) {
        return ArrayOf::empty();
    }
    setObjectProperty(result[0], L"Units", ArrayOf::characterArray(L"points"));
    setObjectProperty(
        result[0], L"Position", ArrayOf::doubleRowVector({ 13.5, 16.875, 243, 11.25 }));
    return result[0];
}
//=============================================================================
static void
updateWaitBarAxes(const ArrayOf& axes, double value)
{
    ArrayOfVector patchArgs;
    patchArgs << ArrayOf::characterArray(L"Parent");
    patchArgs << axes;
    patchArgs << ArrayOf::characterArray(L"XData");
    patchArgs << ArrayOf::doubleRowVector({ 0, value, value, 0 });
    patchArgs << ArrayOf::characterArray(L"YData");
    patchArgs << ArrayOf::doubleRowVector({ 0, 0, 1, 1 });
    patchArgs << ArrayOf::characterArray(L"FaceColor");
    patchArgs << ArrayOf::doubleRowVector({ 0, 0.447, 0.741 });
    patchArgs << ArrayOf::characterArray(L"EdgeColor");
    patchArgs << ArrayOf::doubleRowVector({ 0, 0.447, 0.741 });
    __patch__Builtin(1, patchArgs);
}
//=============================================================================
static ArrayOf
findWaitBarAxes(const ArrayOf& figure)
{
    ArrayOfVector findArgs;
    findArgs << figure;
    findArgs << ArrayOf::characterArray(L"Type");
    findArgs << ArrayOf::characterArray(L"axes");
    ArrayOfVector found = findobjBuiltin(1, findArgs);
    return found.empty() ? ArrayOf::empty() : found[0];
}
//=============================================================================
static void
clearCurrentObject(const ArrayOf& figure)
{
    setObjectProperty(figure, L"CurrentObject", ArrayOf::empty(0, 0));
}
//=============================================================================
static void
setReferenceWaitBarGeometry(const ArrayOf& figure)
{
    if (!figure.isGraphicsObject()) {
        return;
    }
    GOFigure* fig = findGOFigure(figure.asGraphicsObjectScalar());
    if (!fig) {
        return;
    }
    fig->setExposedPositionOverride({ 441, 295.875, 270, 56.25 }, { 441, 295.875, 270, 56.25 },
        { 441, 295.8, 270, 56.4 }, { 5.73749932, 13.85781201, 9.525, 1.984375 });
    fig->setTwoVectorDefault(GO_CURRENT_POINT_PROPERTY_NAME_STR, -0.75, -0.75);
}
//=============================================================================
} // namespace Nelson::GraphicsGateway
//=============================================================================
