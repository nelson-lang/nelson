//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "GOFunctionLine.hpp"
#include "GOArrayOfProperty.hpp"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "GOScalarPositiveIntegerValueProperty.hpp"
#include "GOStringAutoManualProperty.hpp"
#include "GOStringOnOffProperty.hpp"
#include "GOVectorTwoDoubleProperty.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
GOFunctionLine::GOFunctionLine() : GOLineSeries()
{
    registerProperty(new GOArrayOfProperty, GO_FUNCTION_PROPERTY_NAME_STR);
    registerProperty(new GOScalarPositiveIntegerValueProperty, GO_MESH_DENSITY_PROPERTY_NAME_STR);
    registerProperty(new GOTwoVectorProperty, GO_X_RANGE_PROPERTY_NAME_STR);
    registerProperty(new GOAutoManualProperty, GO_X_RANGE_MODE_PROPERTY_NAME_STR);
    registerProperty(new GOOnOffProperty, GO_SHOW_POLES_PROPERTY_NAME_STR);
    setScalarPositiveIntegerValueDefault(GO_MESH_DENSITY_PROPERTY_NAME_STR, 23);
    setTwoVectorDefault(GO_X_RANGE_PROPERTY_NAME_STR, -5, 5);
    setStringDefault(GO_X_RANGE_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setRestrictedStringDefault(GO_SHOW_POLES_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setStringDefault(GO_TYPE_PROPERTY_NAME_STR, getType());
    sortProperties();
}
//=============================================================================
GOFunctionLine::~GOFunctionLine() = default;
//=============================================================================
std::wstring
GOFunctionLine::getType()
{
    return GO_PROPERTY_VALUE_FUNCTION_LINE_STR;
}
//=============================================================================
void
GOFunctionLine::updateState()
{
    if (hasChanged(GO_X_RANGE_PROPERTY_NAME_STR)) {
        toManual(GO_X_RANGE_MODE_PROPERTY_NAME_STR);
    }
    GOLineSeries::updateState();
}
//=============================================================================
} // namespace Nelson
//=============================================================================
