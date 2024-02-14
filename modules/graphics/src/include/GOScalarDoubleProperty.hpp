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
class GOScalarProperty : public GOFixedVectorProperty
{
private:
    bool haveLimits = false;
    double _minValue;
    double _maxValue;

public:
    GOScalarProperty() : GOFixedVectorProperty(1)
    {
        haveLimits = false;
        _minValue = std::nan("NaN");
        _maxValue = std::nan("NaN");
    }
    GOScalarProperty(double minValue, double maxValue) : GOFixedVectorProperty(1)
    {
        haveLimits = true;
        _minValue = minValue;
        _maxValue = maxValue;
    }
    void set(ArrayOf) override;

    ~GOScalarProperty() override = default;
    void
    data(double x);
    double
    data();
    std::wstring
    toWideString() override;
};
//=============================================================================
};
//=============================================================================
