//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "GORestrictedStringScalarProperty.hpp"
#include "GOPropertyValues.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
double
GORestrictedStringScalarProperty::scalar()
{
    return _scalar;
}
//=============================================================================
void
GORestrictedStringScalarProperty::scalar(double scal)
{
    _scalar = scal;
}
//=============================================================================
ArrayOf
GORestrictedStringScalarProperty::get()
{
    if (!isEqual(GO_PROPERTY_VALUE_SCALAR_STR))
        return GORestrictedStringProperty::get();
    return ArrayOf::doubleConstructor(_scalar);
}
//=============================================================================
void
GORestrictedStringScalarProperty::set(ArrayOf arg)
{
    GOGenericProperty::set(arg);
    if (arg.isRowVectorCharacterArray() || (arg.isStringArray() && arg.isScalar())) {
        GORestrictedStringProperty::set(arg);
    } else {
        _scalar = arg.getContentAsDoubleScalar();
    }
}
//=============================================================================
}
//=============================================================================
