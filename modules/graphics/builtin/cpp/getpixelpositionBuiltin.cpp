//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <string>
#include <vector>
#include "getpixelpositionBuiltin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "GORoot.hpp"
#include "NelsonException.hpp"
#include "GOFigure.hpp"
#include "GOGObjectsProperty.hpp"
#include "GOHelpers.hpp"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "GOUIComponentUnits.hpp"
#include "GraphicsEnvironment.hpp"
#include "GraphicsObject.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "UnitsConversionHelpers.hpp"
//=============================================================================
namespace Nelson::GraphicsGateway {
//=============================================================================
static GraphicsObject*
scalarGraphicsObject(const ArrayOf& arg);
static bool
isRecursive(const ArrayOf& arg);
static std::vector<double>
pixelPosition(GraphicsObject* go);
static GraphicsObject*
parentObject(GraphicsObject* go);
static void
raiseInvalidHandle();
//=============================================================================
ArrayOfVector
getpixelpositionBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 2);
    nargoutcheck(nLhs, 0, 1);
    std::vector<double> position(4, 0.0);
    if (!(argIn[0].isEmpty() && argIn[0].isNumeric())) {
        GraphicsObject* go = scalarGraphicsObject(argIn[0]);
        const bool recursive = argIn.size() > 1 && isRecursive(argIn[1]);
        position = pixelPosition(go);
        // Each container up to the figure shifts by its origin and by its border and title.
        GraphicsObject* child = go;
        GraphicsObject* ancestor = recursive ? parentObject(go) : nullptr;
        for (int guard = 0; ancestor && guard < 64; ++guard) {
            if (ancestor->isType(GO_PROPERTY_VALUE_FIGURE_STR)) {
                break;
            }
            // A tiled layout places its axes in the frame of its own parent: no shift.
            if (ancestor->isType(GO_PROPERTY_VALUE_TILED_LAYOUT_STR)) {
                child = ancestor;
                ancestor = parentObject(ancestor);
                continue;
            }
            std::vector<double> ancestorPosition = pixelPosition(ancestor);
            std::vector<double> content;
            double frameHeight = 0;
            if (!uiParentContentRect(child, content, frameHeight) || content.size() != 4) {
                content.assign(4, 0.0);
            }
            // A tab fills its group: the page inset (border and tab bar) is already
            // carried when the tab itself climbs to its group, so the tab adds no
            // further origin offset for its own children here.
            if (ancestor->isType(GO_PROPERTY_VALUE_UITAB_STR)) {
                content[0] = 0;
                content[1] = 0;
            }
            position[0] += ancestorPosition[0] - 1 + content[0];
            position[1] += ancestorPosition[1] - 1 + content[1];
            child = ancestor;
            ancestor = parentObject(ancestor);
        }
    }
    ArrayOfVector retval;
    retval << ArrayOf::doubleRowVector(position);
    return retval;
}
//=============================================================================
// A scalar graphics object, or the number of an open figure.
GraphicsObject*
scalarGraphicsObject(const ArrayOf& arg)
{
    if (!arg.isScalar()) {
        raiseInvalidHandle();
    }
    GraphicsObject* go = nullptr;
    if (arg.isGraphicsObject()) {
        const int64 handle = *static_cast<const int64*>(arg.dataPointer());
        if (isDeletedGraphicsObject(handle)) {
            raiseInvalidHandle();
        }
        if (handle == HANDLE_ROOT_OBJECT) {
            go = getGraphicsRootObject();
        } else if (handle >= HANDLE_OFFSET_OBJECT) {
            go = findGraphicsObject(handle, false);
        } else {
            go = static_cast<GraphicsObject*>(findGOFigure(handle));
        }
    } else if (arg.isNumeric() && !arg.isComplex()) {
        const double number = arg.asDoubleScalar();
        if (number >= 1 && number == static_cast<double>(static_cast<int64>(number))) {
            try {
                go = static_cast<GraphicsObject*>(findGOFigure(static_cast<int64>(number) - 1));
            } catch (const NelsonException&) {
                go = nullptr;
            }
        }
    }
    if (!go) {
        raiseInvalidHandle();
    }
    return go;
}
//=============================================================================
bool
isRecursive(const ArrayOf& arg)
{
    if (!arg.isScalar() || !(arg.isLogical() || arg.isNumeric())) {
        Error(L"Nelson:graphics:getpixelposition:invalidRecursive",
            L"A logical scalar is expected as second argument.");
    }
    return arg.isLogical() ? arg.asLogicalScalar() != 0 : arg.asDoubleScalar() != 0;
}
//=============================================================================
// Position in pixels in the parent; zeros for an object without a four element Position.
std::vector<double>
pixelPosition(GraphicsObject* go)
{
    std::vector<double> zeros(4, 0.0);
    if (!go->haveProperty(GO_POSITION_PROPERTY_NAME_STR)
        || !go->haveProperty(GO_UNITS_PROPERTY_NAME_STR)) {
        return zeros;
    }
    // The value the Position property reports: pixels already start at 1 there.
    ArrayOf reported = go->findProperty(GO_POSITION_PROPERTY_NAME_STR)->get();
    if (!reported.isNumeric() || reported.elementCount() != 4) {
        return zeros;
    }
    reported.promoteType(NLS_DOUBLE);
    const double* values = static_cast<const double*>(reported.dataPointer());
    std::vector<double> position(values, values + 4);
    const std::wstring units = go->findStringProperty(GO_UNITS_PROPERTY_NAME_STR);
    if (units == GO_PROPERTY_VALUE_PIXELS_STR) {
        return position;
    }
    double width = 0;
    double height = 0;
    if (go->isType(GO_PROPERTY_VALUE_FIGURE_STR)) {
        int screenWidth = 0;
        int screenHeight = 0;
        GraphicsEnvironment::currentScreenResolution(screenWidth, screenHeight);
        width = screenWidth;
        height = screenHeight;
    } else if (go->isType(GO_PROPERTY_VALUE_UITAB_STR) && parentObject(go) != nullptr) {
        // A tab is as large as its tab group, tab bar included.
        std::vector<double> groupPosition = pixelPosition(parentObject(go));
        width = groupPosition[2];
        height = groupPosition[3];
    } else {
        std::vector<double> content;
        double frameHeight = 0;
        if (uiParentContentRect(go, content, frameHeight) && content.size() == 4) {
            width = content[2];
            height = content[3];
        } else if (GOFigure* fig = go->getParentFigure()) {
            // The figure Position is in logical pixels, as the Position of its children.
            std::vector<double> figurePosition = pixelPosition(fig);
            width = figurePosition[2];
            height = figurePosition[3];
        }
    }
    return convertPositionPixelOrigin(units, GO_PROPERTY_VALUE_PIXELS_STR, position[0], position[1],
        position[2], position[3], width, height);
}
//=============================================================================
GraphicsObject*
parentObject(GraphicsObject* go)
{
    auto* parent
        = static_cast<GOGObjectsProperty*>(go->findProperty(GO_PARENT_PROPERTY_NAME_STR, false));
    if (!parent || parent->data().empty()) {
        return nullptr;
    }
    const int64 handle = parent->data()[0];
    if (handle == HANDLE_ROOT_OBJECT) {
        return nullptr;
    }
    if (handle < HANDLE_OFFSET_OBJECT) {
        return static_cast<GraphicsObject*>(findGOFigure(handle));
    }
    return findGraphicsObject(handle, false);
}
//=============================================================================
void
raiseInvalidHandle()
{
    Error(L"Nelson:graphics:getpixelposition:invalidHandle",
        L"A valid scalar graphics object is expected as first argument.");
}
//=============================================================================
} // namespace Nelson::GraphicsGateway
//=============================================================================
