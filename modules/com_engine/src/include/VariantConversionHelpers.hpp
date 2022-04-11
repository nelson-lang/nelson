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
#include "ArrayOf.hpp"
#include "nlsCom_engine_exports.h"
#include <Windows.h>
//=============================================================================
namespace Nelson {
//=============================================================================
NLSCOM_ENGINE_IMPEXP bool
ComVariantToNelson(VARIANT* variant, ArrayOf& res, std::wstring& errorMessage);
NLSCOM_ENGINE_IMPEXP bool
NelsonToComVariant(const ArrayOf& A, VARIANT* variant, std::wstring& errorMessage);
//=============================================================================
} // namespace Nelson
//=============================================================================
