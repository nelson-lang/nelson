//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include "GOVectorProperty.hpp"
#include "GOVectorTwoDoubleProperty.hpp"
#include "nlsGraphics_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
class NLSGRAPHICS_IMPEXP GOCategoricalVectorProperty : public GOVectorProperty
{
private:
    ArrayOf _categoricalValue;
    bool _hasCategoricalValue = false;

public:
    GOCategoricalVectorProperty() = default;
    GOCategoricalVectorProperty(bool finiteOnly, bool emptyAsMatrix);
    ~GOCategoricalVectorProperty() override = default;
    ArrayOf
    get() override;
    void set(ArrayOf) override;
    void
    data(const std::vector<double>& m);
    void
    categoricalValue(const ArrayOf& value);
    void
    clearCategoricalValue();
};
//=============================================================================
class NLSGRAPHICS_IMPEXP GOCategoricalTwoVectorProperty : public GOTwoVectorProperty
{
private:
    ArrayOf _categoricalValue;
    bool _hasCategoricalValue = false;

public:
    GOCategoricalTwoVectorProperty() = default;
    ~GOCategoricalTwoVectorProperty() override = default;
    ArrayOf
    get() override;
    void set(ArrayOf) override;
    void
    data(const std::vector<double>& m);
    void
    categoricalValue(const ArrayOf& value);
    void
    clearCategoricalValue();
};
//=============================================================================
} // namespace Nelson
//=============================================================================
