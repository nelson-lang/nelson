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
#include "GOGenericProperty.hpp"
#include "nlsGraphics_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
class NLSGRAPHICS_IMPEXP GOMaximumNumPointsProperty : public GOGenericProperty
{
private:
    double _data { 1000000 };

public:
    GOMaximumNumPointsProperty() = default;
    ~GOMaximumNumPointsProperty() override = default;
    void
    set(ArrayOf arg) override;
    ArrayOf
    get() override;
    std::wstring
    toWideString() override;
    double
    data();
    void
    data(double value);
};
//=============================================================================
} // namespace Nelson
//=============================================================================
