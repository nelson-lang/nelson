//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <cmath>
#include "GOMaximumNumPointsProperty.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
double
GOMaximumNumPointsProperty::data()
{
    return _data;
}
//=============================================================================
void
GOMaximumNumPointsProperty::data(double value)
{
    _data = value;
}
//=============================================================================
ArrayOf
GOMaximumNumPointsProperty::get()
{
    return ArrayOf::doubleScalar(_data);
}
//=============================================================================
void
GOMaximumNumPointsProperty::set(ArrayOf arg)
{
    if (!arg.isNumeric() || arg.isComplex() || !arg.isScalar()) {
        Error(L"Nelson:animatedline:invalidMaximumNumPoints",
            L"MaximumNumPoints must be a positive numeric scalar.");
    }
    double value = arg.asDoubleScalar();
    if (std::isnan(value) || value <= 0) {
        Error(L"Nelson:animatedline:invalidMaximumNumPoints",
            L"MaximumNumPoints must be a positive numeric scalar.");
    }
    GOGenericProperty::set(arg);
    _data = std::isinf(value) ? value : std::ceil(value);
}
//=============================================================================
std::wstring
GOMaximumNumPointsProperty::toWideString()
{
    if (std::isinf(_data)) {
        return L"Inf";
    }
    return std::to_wstring((int64)_data);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
