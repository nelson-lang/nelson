//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "GOScalarPositiveIntegerValueProperty.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
double
GOScalarPositiveIntegerValueProperty::data()
{
    return _data;
}
//=============================================================================
void
GOScalarPositiveIntegerValueProperty::data(double m)
{
    _data = m;
}
//=============================================================================
bool
GOScalarPositiveIntegerValueProperty::isEqual(double m)
{
    return (_data == m);
}
//=============================================================================
std::wstring
GOScalarPositiveIntegerValueProperty::toWideString()
{
    return std::to_wstring((int64)_data);
}
//=============================================================================
ArrayOf
GOScalarPositiveIntegerValueProperty::get()
{
    return ArrayOf::doubleScalar(_data);
}
//=============================================================================
void
GOScalarPositiveIntegerValueProperty::set(ArrayOf arg)
{
    double value = arg.asDoubleScalar();

    if (value != (int)value || !std::isfinite(value) || value < 1) {
        Error("Nelson:graphics:expectedPositiveInteger",
            "Value should be an positive integer value > 0.");
    }
    GOGenericProperty::set(arg);
    _data = value;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
