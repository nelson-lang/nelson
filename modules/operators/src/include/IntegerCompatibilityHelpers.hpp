//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
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
//=============================================================================
namespace Nelson {
//=============================================================================
inline bool
isScalarDoubleOrChar(const ArrayOf& value)
{
    NelsonType type = value.dataClass();
    return value.isScalar() && (type == NLS_DOUBLE || type == NLS_CHAR);
}
//=============================================================================
inline bool
isRealDoubleScalar(const ArrayOf& value)
{
    return value.dataClass() == NLS_DOUBLE && value.isScalar() && !value.isSparse();
}
//=============================================================================
} // namespace Nelson
//=============================================================================
