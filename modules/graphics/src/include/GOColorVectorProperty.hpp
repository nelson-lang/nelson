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
#include "GOVectorTwoDoubleProperty.hpp"
#include "nlsGraphics_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
class NLSGRAPHICS_IMPEXP GOColorVectorProperty : public GOVectorProperty
{
public:
    GOColorVectorProperty() : GOVectorProperty() { }
    ~GOColorVectorProperty() override = default;
    void set(ArrayOf) override;
    ArrayOf
    get() override;
};
//=============================================================================
};
//=============================================================================
