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
class NLSGRAPHICS_IMPEXP GOSeriesIndexProperty : public GOGenericProperty
{
private:
    bool _isNone { true };
    double _value { 1 };

public:
    GOSeriesIndexProperty() = default;
    ~GOSeriesIndexProperty() override = default;
    void
    set(ArrayOf arg) override;
    ArrayOf
    get() override;
    std::wstring
    toWideString() override;
    bool
    isNone();
    double
    value();
    void
    setNone();
    void
    setValue(double value);
};
//=============================================================================
} // namespace Nelson
//=============================================================================
