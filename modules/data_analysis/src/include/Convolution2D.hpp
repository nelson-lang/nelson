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
#include "nlsData_analysis_exports.h"
#include "ArrayOf.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
NLSDATA_ANALYSIS_IMPEXP
ArrayOf
Convolution2D(const ArrayOf& A, const ArrayOf& B, const std::wstring& shape, bool& needToOverload);
//=============================================================================
NLSDATA_ANALYSIS_IMPEXP
ArrayOf
Convolution2D(const ArrayOf& u, const ArrayOf& v, const ArrayOf& A, const std::wstring& shape,
    bool& needToOverload);
//=============================================================================
} // namespace nelson
//=============================================================================
