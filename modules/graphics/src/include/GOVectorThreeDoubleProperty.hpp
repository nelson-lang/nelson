//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
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
class GOThreeVectorProperty : public GOFixedVectorProperty
{
public:
    GOThreeVectorProperty() : GOFixedVectorProperty(3) { }
    ~GOThreeVectorProperty() override = default;
    void
    value(double x, double y, double z);
};
//=============================================================================
};
//=============================================================================
