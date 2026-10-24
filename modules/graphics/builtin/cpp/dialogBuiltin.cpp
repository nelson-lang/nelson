//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "dialogBuiltin.hpp"
#include "figureBuiltin.hpp"
#include "GOFigure.hpp"
#include "GOHelpers.hpp"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "GOScalarDoubleProperty.hpp"
#include "GOVectorProperty.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
namespace Nelson::GraphicsGateway {
//=============================================================================
static void
clearFigureNumber(const ArrayOfVector& result);
//=============================================================================
ArrayOfVector
dialogBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargoutcheck(nLhs, 0, 1);
    ArrayOfVector figureArgs;
    figureArgs << ArrayOf::characterArray(L"MenuBar");
    figureArgs << ArrayOf::characterArray(L"none");
    figureArgs << ArrayOf::characterArray(L"ToolBar");
    figureArgs << ArrayOf::characterArray(L"auto");
    figureArgs << ArrayOf::characterArray(L"NumberTitle");
    figureArgs << ArrayOf::characterArray(L"off");
    figureArgs << ArrayOf::characterArray(GO_INTEGER_HANDLE_PROPERTY_NAME_STR);
    figureArgs << ArrayOf::characterArray(GO_PROPERTY_VALUE_OFF_STR);
    figureArgs << ArrayOf::characterArray(GO_HANDLE_VISIBILITY_PROPERTY_NAME_STR);
    figureArgs << ArrayOf::characterArray(L"callback");
    figureArgs << ArrayOf::characterArray(GO_AUTO_RESIZE_CHILDREN_PROPERTY_NAME_STR);
    figureArgs << ArrayOf::characterArray(GO_PROPERTY_VALUE_OFF_STR);
    figureArgs << ArrayOf::characterArray(GO_DOCK_CONTROLS_PROPERTY_NAME_STR);
    figureArgs << ArrayOf::characterArray(GO_PROPERTY_VALUE_OFF_STR);
    figureArgs << ArrayOf::characterArray(GO_NEXT_PLOT_PROPERTY_NAME_STR);
    figureArgs << ArrayOf::characterArray(L"add");
    figureArgs << ArrayOf::characterArray(GO_COLOR_PROPERTY_NAME_STR);
    figureArgs << ArrayOf::doubleRowVector(
        { 0.96078431372549, 0.96078431372549, 0.96078431372549 });
    figureArgs << ArrayOf::characterArray(GO_BUTTON_DOWN_FCN_PROPERTY_NAME_STR);
    figureArgs << ArrayOf::characterArray(L"if isempty(allchild(gcbf)), close(gcbf), end");
    figureArgs << ArrayOf::characterArray(L"Resize");
    figureArgs << ArrayOf::characterArray(L"off");
    figureArgs << ArrayOf::characterArray(L"WindowStyle");
    figureArgs << ArrayOf::characterArray(L"modal");
    for (const ArrayOf& arg : argIn) {
        figureArgs << arg;
    }
    ArrayOfVector result = figureBuiltin(1, figureArgs);
    clearFigureNumber(result);
    return result;
}
//=============================================================================
static void
clearFigureNumber(const ArrayOfVector& result)
{
    if (result.empty() || !result[0].isGraphicsObject()) {
        return;
    }
    GOFigure* fig = findGOFigure(result[0].asGraphicsObjectScalar());
    if (!fig) {
        return;
    }
    auto* numberProperty
        = static_cast<GOScalarProperty*>(fig->findProperty(GO_NUMBER_PROPERTY_NAME_STR));
    static_cast<GOVectorProperty*>(numberProperty)->data(std::vector<double> {});
}
//=============================================================================
} // namespace Nelson::GraphicsGateway
//=============================================================================
