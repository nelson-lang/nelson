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
#include "ArrayOf.hpp"
#include "nlsString_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
NLSSTRING_IMPEXP std::wstring
ToChar(const ArrayOf& A, Dimensions& dims);
//=============================================================================
NLSSTRING_IMPEXP std::wstring
ToChar(const ArrayOfVector& A, Dimensions& dims);
//=============================================================================
NLSSTRING_IMPEXP ArrayOf
ToChar(const ArrayOf& A, const ArrayOf& B, bool& needToOverload);
//=============================================================================
NLSSTRING_IMPEXP ArrayOf
ToChar(const ArrayOf& A, bool& needToOverload);
//=============================================================================
} // namespace Nelson
  //=============================================================================
