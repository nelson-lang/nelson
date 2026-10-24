//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "GOScalarLogicalProperty.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
GOScalarLogicalProperty::get()
{
    return ArrayOf::logicalScalar(_data);
}
//=============================================================================
void
GOScalarLogicalProperty::set(ArrayOf num)
{
    GOGenericProperty::set(num);
    if (num.isScalar() && (num.isLogical() || num.isNumeric())) {
        _data = (num.asDoubleScalar() != 0);
    } else {
        Error(L"Nelson:graphics:expectedLogicalScalar",
            L"Value must be a logical or numeric scalar (0 or 1).");
    }
}
//=============================================================================
std::wstring
GOScalarLogicalProperty::toWideString()
{
    return _data ? L"1" : L"0";
}
//=============================================================================
} // namespace Nelson
//=============================================================================
