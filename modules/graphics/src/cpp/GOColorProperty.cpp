//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "GOColorProperty.hpp"
#include "Error.hpp"
#include "PredefinedErrorMessages.hpp"
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
        raiseError(L"Nelson:graphics:ERROR_EXPECTING_A_COLOR_SPEC_EITHER_A_COLOR_NAME_OR_A_3_"
                   L"VECTOR_OR_RGB_VALUES",
            ERROR_EXPECTING_A_COLOR_SPEC_EITHER_A_COLOR_NAME_OR_A_3_VECTOR_OR_RGB_VALUES);
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
