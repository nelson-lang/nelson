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
#include <cstdio>
#include <string>
#include "nlsStream_manager_exports.h"
#include "ArrayOf.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
NLSSTREAM_MANAGER_IMPEXP ArrayOf
SscanF(const std::wstring& content, const std::wstring& format, double m, double n,
    bool haveThirdArgument, indexType& count, indexType& nextIndex, std::wstring& errorMessage);
//=============================================================================
}
//=============================================================================
