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
#include "GORestrictedStringProperty.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
class GORestrictedStringColorProperty : public GORestrictedStringProperty
{
    std::vector<double> colorspec;

public:
    GORestrictedStringColorProperty(const wchar_t** dict) : GORestrictedStringProperty(dict) { }
    ~GORestrictedStringColorProperty() override = default;
    void set(ArrayOf) override;
    ArrayOf
    get() override;
    std::vector<double>
    colorSpec();
    void
    colorSpec(const std::vector<double>& col);
    void
    colorSpec(double r, double g, double b);
};
//=============================================================================
};
//=============================================================================
