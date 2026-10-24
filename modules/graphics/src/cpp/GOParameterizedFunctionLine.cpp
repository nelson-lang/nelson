//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "GOParameterizedFunctionLine.hpp"
#include "GOArrayOfProperty.hpp"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "GOScalarPositiveIntegerValueProperty.hpp"
#include "GOStringAutoManualProperty.hpp"
#include "GOVectorTwoDoubleProperty.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
GOParameterizedFunctionLine::GOParameterizedFunctionLine() : GOLineSeries()
{
    registerProperty(new GOArrayOfProperty, GO_X_FUNCTION_PROPERTY_NAME_STR);
    registerProperty(new GOArrayOfProperty, GO_Y_FUNCTION_PROPERTY_NAME_STR);
    registerProperty(new GOArrayOfProperty, GO_Z_FUNCTION_PROPERTY_NAME_STR);
    registerProperty(new GOScalarPositiveIntegerValueProperty, GO_MESH_DENSITY_PROPERTY_NAME_STR);
    registerProperty(new GOTwoVectorProperty, GO_T_RANGE_PROPERTY_NAME_STR);
    registerProperty(new GOAutoManualProperty, GO_T_RANGE_MODE_PROPERTY_NAME_STR);
    setScalarPositiveIntegerValueDefault(GO_MESH_DENSITY_PROPERTY_NAME_STR, 15);
    setTwoVectorDefault(GO_T_RANGE_PROPERTY_NAME_STR, -5, 5);
    setStringDefault(GO_T_RANGE_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setStringDefault(GO_TYPE_PROPERTY_NAME_STR, getType());
    sortProperties();
}
//=============================================================================
GOParameterizedFunctionLine::~GOParameterizedFunctionLine() = default;
//=============================================================================
std::wstring
GOParameterizedFunctionLine::getType()
{
    return GO_PROPERTY_VALUE_PARAMETERIZED_FUNCTION_LINE_STR;
}
//=============================================================================
void
GOParameterizedFunctionLine::updateState()
{
    if (hasChanged(GO_T_RANGE_PROPERTY_NAME_STR)) {
        toManual(GO_T_RANGE_MODE_PROPERTY_NAME_STR);
    }
    GOLineSeries::updateState();
}
//=============================================================================
} // namespace Nelson
//=============================================================================
