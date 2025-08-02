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
#include "nlsOperators_exports.h"
#include "ArrayOf.hpp"
//=============================================================================
namespace Nelson {
ArrayOf
relationOperator(const ArrayOf& A, const ArrayOf& B, const std::wstring& operatorName,
    logical (*realRelationOperator)(
        NelsonType commonClass, void* vptrA, void* vptrB, indexType idxA, indexType idxB),
    logical (*complexRelationOperator)(
        NelsonType commonClass, void* vptrA, void* vptrB, indexType idxA, indexType idxB),
    logical (*stringRelationOperator)(
        NelsonType commonClass, void* vptrA, void* vptrB, indexType idxA, indexType idxB),
    bool& needToOverload);
} // namespace Nelson
//=============================================================================
