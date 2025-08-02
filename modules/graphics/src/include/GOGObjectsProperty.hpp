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
#include <vector>
#include "nlsGraphics_exports.h"
#include "GOGenericProperty.hpp"
#include "ArrayOf.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
class NLSGRAPHICS_IMPEXP GOGObjectsProperty : public GOGenericProperty
{
protected:
    std::vector<int64> _data;

public:
    GOGObjectsProperty();
    ~GOGObjectsProperty() override = default;
    ArrayOf
    get() override;
    void set(ArrayOf) override;
    std::vector<int64>
    data();
    void
    data(const std::vector<int64>& m);
    std::wstring
    toWideString() override;
};
//=============================================================================
};
//=============================================================================
