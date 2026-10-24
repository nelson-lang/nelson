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
#include "resetBuiltin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "NelsonException.hpp"
#include "GOAnimatedLine.hpp"
#include "GOAxis.hpp"
#include "GOColorProperty.hpp"
#include "GOFigure.hpp"
#include "GOHelpers.hpp"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "GORoot.hpp"
#include "GraphicsObjectFactory.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
namespace Nelson::GraphicsGateway {
//=============================================================================
static bool
isPolarAxis(GraphicsObject* object)
{
    GOGenericProperty* polarProperty = object->findProperty(GO_POLAR_AXES_PROPERTY_NAME_STR, false);
    return polarProperty && polarProperty->get().asWideString() == GO_PROPERTY_VALUE_ON_STR;
}
//=============================================================================
static void
resetFigure(GOFigure* fig)
{
    fig->setThreeVectorDefault(GO_COLOR_PROPERTY_NAME_STR, GOFigure::LIGHT_BACKGROUND_GRAY,
        GOFigure::LIGHT_BACKGROUND_GRAY, GOFigure::LIGHT_BACKGROUND_GRAY);
    fig->setStringDefault(GO_NAME_PROPERTY_NAME_STR, {});
    fig->setRestrictedStringDefault(GO_NEXT_PLOT_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_REPLACE_STR);
    fig->setRestrictedStringDefault(GO_VISIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    fig->setRestrictedStringDefault(
        GO_HANDLE_VISIBILITY_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    fig->setRestrictedStringDefault(GO_NUMBER_TITLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    fig->setRestrictedStringDefault(
        GO_GRAPHICS_SMOOTHING_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    fig->setRestrictedStringDefault(GO_INTERRUPTIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    fig->setRestrictedStringDefault(GO_BUSY_ACTION_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_QUEUE_STR);
    fig->setRestrictedStringDefault(
        GO_WINDOW_STATE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_NORMAL_STR);
    fig->setRestrictedStringDefault(GO_RESIZE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
}
//=============================================================================
static void
resetObject(GraphicsObject* object)
{
    if (object->isType(GO_PROPERTY_VALUE_ROOT_STR)) {
        static_cast<GORoot*>(object)->resetProperties();
        return;
    }
    if (object->isType(GO_PROPERTY_VALUE_FIGURE_STR)) {
        resetFigure(static_cast<GOFigure*>(object));
        object->updateState();
        return;
    }

    bool keepPolarAxis = object->isType(GO_PROPERTY_VALUE_AXES_STR) && isPolarAxis(object);
    std::unique_ptr<GraphicsObject> defaults(
        newGraphicsObjectForType(object->getType(), nullptr, true));
    if (!defaults) {
        Error(L"Nelson:graphics:unsupportedType", L"Unsupported graphics object type.");
    }
    for (const auto& name : defaults->fieldnames()) {
        if (!canCopyGraphicsProperty(defaults.get(), object, name)) {
            continue;
        }
        try {
            object->findProperty(name)->set(defaults->findProperty(name)->get());
        } catch (const NelsonException&) {
        }
    }
    if (object->isType(GO_PROPERTY_VALUE_AXES_STR)) {
        if (keepPolarAxis) {
            object->findProperty(GO_POLAR_AXES_PROPERTY_NAME_STR)
                ->set(ArrayOf::stringArray(GO_PROPERTY_VALUE_ON_STR));
        }
        int64 handle = findGraphicsObjectHandle(object);
        if (handle >= HANDLE_OFFSET_OBJECT) {
            static_cast<GOAxis*>(object)->restoreToolbar(handle);
        }
        // Non-writable yyaxis state is not copied by the loop above, so drop the
        // second y-ruler explicitly.
        static_cast<GOAxis*>(object)->disableDualYAxis();
    } else if (object->isType(GO_PROPERTY_VALUE_ANIMATED_LINE_STR)) {
        static_cast<GOAnimatedLine*>(object)->clearPoints();
    }
    object->updateState();
}
//=============================================================================
ArrayOfVector
resetBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 0);
    if (!argIn[0].isGraphicsObject()) {
        Error(L"Nelson:graphics:expectedGraphicsObject:expectedGraphicsObjectS",
            L"Expected graphics object(s).");
    }
    const nelson_handle* handles = static_cast<const nelson_handle*>(argIn[0].dataPointer());
    for (indexType k = 0; k < argIn[0].elementCount(); ++k) {
        resetObject(graphicsObjectFromHandle(handles[k], true));
    }
    return {};
}
//=============================================================================
} // namespace Nelson::GraphicsGateway
//=============================================================================
