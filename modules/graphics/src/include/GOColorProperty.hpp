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
#if _MSC_VER
#pragma warning(disable : 4275)
#endif
//=============================================================================
#include "nlsGraphics_exports.h"
//=============================================================================
#include "GOFixedVectorProperty.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
class NLSGRAPHICS_IMPEXP GOColorProperty : public GOFixedVectorProperty
{
public:
    GOColorProperty() : GOFixedVectorProperty(3) { }
    ~GOColorProperty() override = default;
    void set(ArrayOf) override;
    ArrayOf
    get() override;
    bool
    isNone();
};
//=============================================================================
} // namespace Nelson
//=============================================================================
