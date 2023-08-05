//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include "HandleGenericObject.hpp"
#include "nlsCom_engine_exports.h"
#include <string>
//=============================================================================
namespace Nelson {
//=============================================================================
class NLSCOM_ENGINE_IMPEXP ComHandleObject : public HandleGenericObject
{
public:
    ComHandleObject(void* _ptr);
    ~ComHandleObject() override;
};
//=============================================================================
} // namespace Nelson
//=============================================================================
