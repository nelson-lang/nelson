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
#include "nlsGraphics_exports.h"
#include "GOGenericProperty.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
// Scalar logical property (compatible 'Value' of state buttons,
// check boxes, radio buttons, ...): accepts logical or numeric scalar,
// always returned as logical.
class NLSGRAPHICS_IMPEXP GOScalarLogicalProperty : public GOGenericProperty
{
protected:
    bool _data = false;

public:
    GOScalarLogicalProperty() = default;
    GOScalarLogicalProperty(bool defaultValue) : _data(defaultValue) { }
    ~GOScalarLogicalProperty() override = default;
    ArrayOf
    get() override;
    void set(ArrayOf) override;
    bool
    data() const
    {
        return _data;
    }
    void
    data(bool m)
    {
        _data = m;
    }
    std::wstring
    toWideString() override;
};
//=============================================================================
} // namespace Nelson
//=============================================================================
