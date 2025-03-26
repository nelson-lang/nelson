//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "GORestrictedStringColorProperty.hpp"
#include "GOColorHelpers.hpp"
#include "GOPropertyValues.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
std::vector<double>
GORestrictedStringColorProperty::colorSpec()
{
    return colorspec;
}
//=============================================================================
void
GORestrictedStringColorProperty::colorSpec(const std::vector<double>& col)
{
    colorspec = col;
}
//=============================================================================
void
GORestrictedStringColorProperty::colorSpec(double r, double g, double b)
{
    std::vector<double> data;
    data.reserve(3);
    data.push_back(r);
    data.push_back(g);
    data.push_back(b);
    colorspec = data;
}
//=============================================================================
void
GORestrictedStringColorProperty::set(ArrayOf arg)
{
    GOGenericProperty::set(arg);
    if (!ParseColorToRGB(arg, colorspec)) {
        GORestrictedStringProperty::set(arg);
    } else {
        _data = GO_PROPERTY_VALUE_COLORSPEC_STR;
    }
    if (colorspec.size() > 1 && (colorspec[0] == -1)) {
        _data = GO_PROPERTY_VALUE_NONE_STR;
    }
}
//=============================================================================
ArrayOf
GORestrictedStringColorProperty::get()
{
    if (!isEqual(GO_PROPERTY_VALUE_COLORSPEC_STR)) {
        return GORestrictedStringProperty::get();
    }
    ArrayOf ret(ArrayOf::doubleVectorConstructor(3));
    double* dp = (double*)ret.getReadWriteDataPointer();
    dp[0] = colorspec[0];
    dp[1] = colorspec[1];
    dp[2] = colorspec[2];
    return ret;
}
//=============================================================================
}
//=============================================================================
