//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include "GOFixedVectorProperty.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
class GOSixVectorProperty : public GOFixedVectorProperty
{
public:
    GOSixVectorProperty() : GOFixedVectorProperty(6) { }
    ~GOSixVectorProperty() override = default;
    void
    value(double x1, double x2, double y1, double y2, double z1, double z2);
    std::wstring
    toWideString() override;
};
//=============================================================================
};
//=============================================================================
