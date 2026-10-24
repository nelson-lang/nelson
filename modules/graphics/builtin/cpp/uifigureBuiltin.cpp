//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "uifigureBuiltin.hpp"
#include "figureBuiltin.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "GOFigure.hpp"
#include "GOHelpers.hpp"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "GOScalarDoubleProperty.hpp"
#include "GOVectorProperty.hpp"
#include "StringHelpers.hpp"
//=============================================================================
namespace Nelson::GraphicsGateway {
//=============================================================================
static ArrayOfVector
defaultUiFigureProperties();
static bool
hasPositionArgument(const ArrayOfVector& argIn);
//=============================================================================
ArrayOfVector
uifigureBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargoutcheck(nLhs, 0, 1);
    ArrayOfVector figureArgs = defaultUiFigureProperties();
    for (const ArrayOf& arg : argIn) {
        figureArgs << arg;
    }
    ArrayOfVector result = figureBuiltin(1, figureArgs);
    if (!result.empty() && result[0].isGraphicsObject()) {
        GOFigure* fig = findGOFigure(result[0].asGraphicsObjectScalar());
        if (fig) {
            auto* numberProperty
                = static_cast<GOScalarProperty*>(fig->findProperty(GO_NUMBER_PROPERTY_NAME_STR));
            static_cast<GOVectorProperty*>(numberProperty)->data(std::vector<double> {});
            if (!hasPositionArgument(argIn)) {
                fig->setExposedPositionOverride(
                    { 488, 242, 560, 420 }, { 488, 242, 560, 420 }, { 488.2, 241.8, 560, 420 }, {});
            }
        }
    }
    return result;
}
//=============================================================================
static ArrayOfVector
defaultUiFigureProperties()
{
    ArrayOfVector args;
    args << ArrayOf::characterArray(L"MenuBar");
    args << ArrayOf::characterArray(L"none");
    args << ArrayOf::characterArray(L"ToolBar");
    args << ArrayOf::characterArray(L"none");
    args << ArrayOf::characterArray(L"NumberTitle");
    args << ArrayOf::characterArray(L"off");
    args << ArrayOf::characterArray(L"Resize");
    args << ArrayOf::characterArray(L"on");
    args << ArrayOf::characterArray(L"WindowStyle");
    args << ArrayOf::characterArray(L"normal");
    args << ArrayOf::characterArray(GO_AUTO_RESIZE_CHILDREN_PROPERTY_NAME_STR);
    args << ArrayOf::characterArray(GO_PROPERTY_VALUE_ON_STR);
    args << ArrayOf::characterArray(GO_DOCK_CONTROLS_PROPERTY_NAME_STR);
    args << ArrayOf::characterArray(GO_PROPERTY_VALUE_OFF_STR);
    args << ArrayOf::characterArray(GO_NEXT_PLOT_PROPERTY_NAME_STR);
    args << ArrayOf::characterArray(L"add");
    args << ArrayOf::characterArray(GO_COLOR_PROPERTY_NAME_STR);
    args << ArrayOf::doubleRowVector({ 0.96078431372549, 0.96078431372549, 0.96078431372549 });
    args << ArrayOf::characterArray(GO_HANDLE_VISIBILITY_PROPERTY_NAME_STR);
    args << ArrayOf::characterArray(GO_PROPERTY_VALUE_OFF_STR);
    args << ArrayOf::characterArray(GO_INTEGER_HANDLE_PROPERTY_NAME_STR);
    args << ArrayOf::characterArray(GO_PROPERTY_VALUE_OFF_STR);
    args << ArrayOf::characterArray(L"Position");
    args << ArrayOf::doubleRowVector({ 488, 242, 560, 420 });
    return args;
}
//=============================================================================
static bool
hasPositionArgument(const ArrayOfVector& argIn)
{
    for (size_t k = 0; k + 1 < argIn.size(); k += 2) {
        if (argIn[k].isCharacterArray()) {
            std::wstring name = argIn[k].asWideString();
            StringHelpers::to_lower(name);
            if (name == L"position" || name == L"innerposition" || name == L"outerposition") {
                return true;
            }
        }
    }
    return false;
}
//=============================================================================
} // namespace Nelson::GraphicsGateway
//=============================================================================
