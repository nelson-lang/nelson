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
public:
    GOScalarProperty() : GOFixedVectorProperty(1) { }
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
