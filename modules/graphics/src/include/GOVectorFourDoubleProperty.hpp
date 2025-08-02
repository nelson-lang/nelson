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
class GOFourVectorProperty : public GOFixedVectorProperty
{
public:
    GOFourVectorProperty() : GOFixedVectorProperty(4) { }
    ~GOFourVectorProperty() override = default;
    void
    value(double x, double y, double z, double w);
    std::wstring
    toWideString() override;
};
//=============================================================================
};
//=============================================================================
