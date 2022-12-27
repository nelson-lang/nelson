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
class GOTwoVectorProperty : public GOFixedVectorProperty
{
public:
    GOTwoVectorProperty() : GOFixedVectorProperty(2) { }
    ~GOTwoVectorProperty() override = default;
    void
    value(double x, double y);
};
//=============================================================================
};
//=============================================================================
