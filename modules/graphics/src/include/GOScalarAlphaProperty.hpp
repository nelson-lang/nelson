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
class GOScalarAlphaProperty : public GOFixedVectorProperty
{
private:
    double _minValue;
    double _maxValue;
    bool _isFlat;

public:
#define DEFAULT_ALPHA 1
    GOScalarAlphaProperty() : GOFixedVectorProperty(DEFAULT_ALPHA)
    {
        _isFlat = false;
        _minValue = 0;
        _maxValue = 1;
    }
    void set(ArrayOf) override;
    ~GOScalarAlphaProperty() override = default;
    void
    data(double x);
    double
    data();
    bool
    isFlat();
    std::wstring
    toWideString() override;
};
//=============================================================================
};
//=============================================================================
