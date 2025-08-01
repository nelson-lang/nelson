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
#include <matio.h>
#include "ArrayOf.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
LoadMatioSingle(matvar_t* matVariable, ArrayOf& VariableValue);
//=============================================================================
} // namespace Nelson
//=============================================================================
