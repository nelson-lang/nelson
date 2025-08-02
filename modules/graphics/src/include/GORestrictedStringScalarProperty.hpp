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
#include "GORestrictedStringScalarProperty.hpp"
#include "GORestrictedStringProperty.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
class GORestrictedStringScalarProperty : public GORestrictedStringProperty
{
private:
    double _scalar { 0 };

public:
    GORestrictedStringScalarProperty(const wchar_t** dict) : GORestrictedStringProperty(dict) { }
    ~GORestrictedStringScalarProperty() override = default;
    void set(ArrayOf) override;
    ArrayOf
    get() override;
    double
    scalar();
    void
    scalar(double val);
};
//=============================================================================
};
//=============================================================================
