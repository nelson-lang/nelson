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
//=============================================================================
namespace Nelson {
//=============================================================================
class GOScalarPositiveIntegerValueProperty : public GOGenericProperty
{
protected:
    double _data;

public:
    GOScalarPositiveIntegerValueProperty() = default;
    ~GOScalarPositiveIntegerValueProperty() override = default;
    ArrayOf
    get() override;
    void set(ArrayOf) override;
    double
    data();
    void
    data(double m);
    bool
    isEqual(double m);
    std::wstring
    toWideString() override;
};
//=============================================================================
};
//=============================================================================
