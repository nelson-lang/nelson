//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "GOAxesToolbarProperty.hpp"
#include "GOHelpers.hpp"
#include "GOPropertyValues.hpp"
#include "GraphicsObject.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
void
GOAxesToolbarProperty::set(ArrayOf arg)
{
    if (arg.isEmpty()) {
        GOGObjectsProperty::set(arg);
        return;
    }
    if (!arg.isGraphicsObject() || !arg.isScalar()) {
        Error(L"Nelson:axes:invalidToolbar", L"Toolbar must be an axes toolbar handle or empty.");
    }
    int64 handle = arg.asGraphicsObjectScalar();
    GraphicsObject* object = graphicsObjectFromHandle(handle, true);
    if (!object || !object->isType(GO_PROPERTY_VALUE_AXES_TOOLBAR_STR)) {
        Error(L"Nelson:axes:invalidToolbar", L"Toolbar must be an axes toolbar handle or empty.");
    }
    GOGObjectsProperty::set(arg);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
