//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "GOLayoutOptions.hpp"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "GOArrayOfProperty.hpp"
#include "GOScalarDoubleProperty.hpp"
#include "GOGObjectsProperty.hpp"
#include "GOVectorTwoDoubleProperty.hpp"
#include "GOStringProperty.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
GOLayoutOptions::GOLayoutOptions()
{
    registerProperties();
    // Defaults
    findProperty(GO_TILE_PROPERTY_NAME_STR)->set(ArrayOf::doubleConstructor(0.0));
    setTwoVectorDefault(GO_TILE_SPAN_PROPERTY_NAME_STR, 1.0, 1.0);
    setStringDefault(GO_TYPE_PROPERTY_NAME_STR, getType());
}
//=============================================================================
std::wstring
GOLayoutOptions::getType()
{
    return L"layoutoptions";
}
//=============================================================================
void
GOLayoutOptions::registerProperties()
{
    registerProperty(new GOStringProperty, GO_TYPE_PROPERTY_NAME_STR, false);
    // Tile: scalar double (0 = auto/unset) or a string for edge tiles
    registerProperty(new GOArrayOfProperty, GO_TILE_PROPERTY_NAME_STR);
    // TileSpan: [rows cols]
    registerProperty(new GOTwoVectorProperty, GO_TILE_SPAN_PROPERTY_NAME_STR);
    registerProperty(new GOGObjectsProperty, GO_PARENT_PROPERTY_NAME_STR);
    sortProperties();
}
//=============================================================================
} // namespace Nelson
//=============================================================================
