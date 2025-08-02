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
#include <string>
#include "Evaluator.hpp"
#include "TestTags.hpp"
#include "Types.hpp"
#include "nlsTests_manager_exports.h"
//=============================================================================
namespace Nelson {
NLSTESTS_MANAGER_IMPEXP bool
ParseTags(const std::wstring& filename, TestTags& options, std::wstring& msg);
}
//=============================================================================
