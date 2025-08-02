//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "GOVectorSixDoubleProperty.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
void
GOSixVectorProperty::value(double x1, double x2, double y1, double y2, double z1, double z2)
{
    at(0) = x1;
    at(1) = x2;
    at(2) = y1;
    at(3) = y2;
    at(4) = z1;
    at(5) = z2;
}
//=============================================================================
std::wstring
GOSixVectorProperty::toWideString()
{
    return L"[" + std::to_wstring((at(0))) + L" " + std::to_wstring((at(1))) + L" "
        + std::to_wstring((at(2))) + L" " + std::to_wstring((at(3))) + L" "
        + std::to_wstring((at(4))) + L" " + std::to_wstring((at(5))) + L"]";
}
//=============================================================================
}
//=============================================================================
