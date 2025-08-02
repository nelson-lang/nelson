//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "DotLeftDivide.hpp"
#include "DotRightDivide.hpp"
#include "MatrixCheck.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
DotLeftDivide(const ArrayOf& A, const ArrayOf& B, bool& needToOverload)
{
    return DotRightDivide(B, A, needToOverload);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
