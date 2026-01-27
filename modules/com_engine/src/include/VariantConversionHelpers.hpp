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
#define WIN32_LEAN_AND_MEAN
#include <Windows.h>
#include <oaidl.h>
#include <oleauto.h>
#include "ArrayOf.hpp"
#include "nlsCom_engine_exports.h"
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
