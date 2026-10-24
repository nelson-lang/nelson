//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "GOImplicitFunctionSurface.hpp"
#include "GOArrayOfProperty.hpp"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "GOScalarPositiveIntegerValueProperty.hpp"
#include "GOStringAutoManualProperty.hpp"
#include "GOVectorTwoDoubleProperty.hpp"
#include <algorithm>
#include <cmath>
//=============================================================================
namespace Nelson {
//=============================================================================
GOImplicitFunctionSurface::GOImplicitFunctionSurface() : GOPatch()
{
    registerProperty(new GOArrayOfProperty, GO_FUNCTION_PROPERTY_NAME_STR);
    registerProperty(new GOScalarPositiveIntegerValueProperty, GO_MESH_DENSITY_PROPERTY_NAME_STR);
    registerProperty(new GOTwoVectorProperty, GO_X_RANGE_PROPERTY_NAME_STR);
    registerProperty(new GOAutoManualProperty, GO_X_RANGE_MODE_PROPERTY_NAME_STR);
    registerProperty(new GOTwoVectorProperty, GO_Y_RANGE_PROPERTY_NAME_STR);
    registerProperty(new GOAutoManualProperty, GO_Y_RANGE_MODE_PROPERTY_NAME_STR);
    registerProperty(new GOTwoVectorProperty, GO_Z_RANGE_PROPERTY_NAME_STR);
    registerProperty(new GOAutoManualProperty, GO_Z_RANGE_MODE_PROPERTY_NAME_STR);
    setScalarPositiveIntegerValueDefault(GO_MESH_DENSITY_PROPERTY_NAME_STR, 35);
    setTwoVectorDefault(GO_X_RANGE_PROPERTY_NAME_STR, -5, 5);
    setStringDefault(GO_X_RANGE_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setTwoVectorDefault(GO_Y_RANGE_PROPERTY_NAME_STR, -5, 5);
    setStringDefault(GO_Y_RANGE_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setTwoVectorDefault(GO_Z_RANGE_PROPERTY_NAME_STR, -5, 5);
    setStringDefault(GO_Z_RANGE_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setRestrictedStringColorDefault(
        GO_FACE_COLOR_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_COLORSPEC_STR, 0, 0.4470, 0.7410);
    setRestrictedStringColorDefault(
        GO_EDGE_COLOR_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_COLORSPEC_STR, 0, 0, 0);
    setStringDefault(GO_TYPE_PROPERTY_NAME_STR, getType());
    sortProperties();
}
//=============================================================================
GOImplicitFunctionSurface::~GOImplicitFunctionSurface() = default;
//=============================================================================
std::wstring
GOImplicitFunctionSurface::getType()
{
    return GO_PROPERTY_VALUE_IMPLICIT_FUNCTION_SURFACE_STR;
}
//=============================================================================
void
GOImplicitFunctionSurface::updateState()
{
    if (hasChanged(GO_X_RANGE_PROPERTY_NAME_STR)) {
        toManual(GO_X_RANGE_MODE_PROPERTY_NAME_STR);
    }
    if (hasChanged(GO_Y_RANGE_PROPERTY_NAME_STR)) {
        toManual(GO_Y_RANGE_MODE_PROPERTY_NAME_STR);
    }
    if (hasChanged(GO_Z_RANGE_PROPERTY_NAME_STR)) {
        toManual(GO_Z_RANGE_MODE_PROPERTY_NAME_STR);
    }
    GOPatch::updateState();
}
//=============================================================================
std::vector<double>
GOImplicitFunctionSurface::getLimits()
{
    std::vector<double> limits = GOPatch::getLimits();
    if (limits.size() < 6) {
        return limits;
    }
    for (size_t k = 0; k < 6; k += 2) {
        double lower = limits[k];
        double upper = limits[k + 1];
        double span = upper - lower;
        if (!std::isfinite(span) || span <= 0) {
            continue;
        }
        double margin = span * 0.0502;
        limits[k] = lower - margin;
        limits[k + 1] = upper + margin;
    }
    return limits;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
