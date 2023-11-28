//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "GOColorProperty.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "GOColorHelpers.hpp"
#include "GOPropertyValues.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
GOColorProperty::isNone()
{
    if (_data.size() > 0) {
        return (_data[0] < 0);
    }
    return true;
}
//=============================================================================
void
GOColorProperty::set(ArrayOf arg)
{
    GOGenericProperty::set(arg);
    if (!ParseColorToRGB(arg, _data)) {
        Error(_W("Expecting a color spec: either a color name or a 3-vector or RGB values"));
    }
}
//=============================================================================
ArrayOf
GOColorProperty::get()
{
    if (_data[0] == -1) {
        return ArrayOf::characterArrayConstructor(GO_PROPERTY_VALUE_NONE_STR);
    }
    return GOVectorProperty::get();
}
//=============================================================================
}
//=============================================================================
