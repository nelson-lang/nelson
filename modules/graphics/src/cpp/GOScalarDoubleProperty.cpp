//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "GOScalarDoubleProperty.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
void
GOScalarProperty::data(double x)
{
    at(0) = x;
}
//=============================================================================
double
GOScalarProperty::data()
{
    return at(0);
}
//=============================================================================
std::wstring
GOScalarProperty::toWideString()
{
    double value = at(0);

    if (std::floor(value) - value < std::numeric_limits<double>::epsilon()) {
        return std::to_wstring((int64)at(0));
    }
    return std::to_wstring(at(0));
}
//=============================================================================
}
