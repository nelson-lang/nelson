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
#include "Evaluator.hpp"
//=============================================================================
namespace Nelson::AssertFunctionsGateway {
//=============================================================================
ArrayOfVector
assert_checkerror_containsBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
//=============================================================================
ArrayOfVector
assert_checkwarningBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
//=============================================================================
ArrayOfVector
assert_sizeBuiltin(int nLhs, const ArrayOfVector& argIn);
//=============================================================================
ArrayOfVector
assert_classBuiltin(int nLhs, const ArrayOfVector& argIn);
//=============================================================================
ArrayOfVector
assert_emptyBuiltin(int nLhs, const ArrayOfVector& argIn);
//=============================================================================
ArrayOfVector
assert_notemptyBuiltin(int nLhs, const ArrayOfVector& argIn);
//=============================================================================
ArrayOfVector
assert_alltrueBuiltin(int nLhs, const ArrayOfVector& argIn);
//=============================================================================
ArrayOfVector
assert_allfalseBuiltin(int nLhs, const ArrayOfVector& argIn);
//=============================================================================
ArrayOfVector
assert_containsBuiltin(int nLhs, const ArrayOfVector& argIn);
//=============================================================================
ArrayOfVector
assert_matchBuiltin(int nLhs, const ArrayOfVector& argIn);
//=============================================================================
ArrayOfVector
assert_typeBuiltin(int nLhs, const ArrayOfVector& argIn);
//=============================================================================
ArrayOfVector
assert_scalarBuiltin(int nLhs, const ArrayOfVector& argIn);
//=============================================================================
ArrayOfVector
assert_vectorBuiltin(int nLhs, const ArrayOfVector& argIn);
//=============================================================================
ArrayOfVector
assert_matrixBuiltin(int nLhs, const ArrayOfVector& argIn);
//=============================================================================
ArrayOfVector
assert_realBuiltin(int nLhs, const ArrayOfVector& argIn);
//=============================================================================
ArrayOfVector
assert_finiteBuiltin(int nLhs, const ArrayOfVector& argIn);
//=============================================================================
ArrayOfVector
assert_nonNanBuiltin(int nLhs, const ArrayOfVector& argIn);
//=============================================================================
ArrayOfVector
assert_sameSizeBuiltin(int nLhs, const ArrayOfVector& argIn);
//=============================================================================
ArrayOfVector
assert_numelBuiltin(int nLhs, const ArrayOfVector& argIn);
//=============================================================================
ArrayOfVector
assert_throwsBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
//=============================================================================
ArrayOfVector
assert_warningFreeBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
//=============================================================================
ArrayOfVector
assert_diffBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
//=============================================================================
ArrayOfVector
assert_failBuiltin(int nLhs, const ArrayOfVector& argIn);
//=============================================================================
ArrayOfVector
assert_noErrorBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
//=============================================================================
ArrayOfVector
assert_warningBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
//=============================================================================
ArrayOfVector
assert_notEqualBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
//=============================================================================
ArrayOfVector
assert_notApproxBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
//=============================================================================
ArrayOfVector
assert_lessThanBuiltin(int nLhs, const ArrayOfVector& argIn);
//=============================================================================
ArrayOfVector
assert_lessOrEqualBuiltin(int nLhs, const ArrayOfVector& argIn);
//=============================================================================
ArrayOfVector
assert_greaterThanBuiltin(int nLhs, const ArrayOfVector& argIn);
//=============================================================================
ArrayOfVector
assert_greaterOrEqualBuiltin(int nLhs, const ArrayOfVector& argIn);
//=============================================================================
ArrayOfVector
assert_inRangeBuiltin(int nLhs, const ArrayOfVector& argIn);
//=============================================================================
ArrayOfVector
assert_rowVectorBuiltin(int nLhs, const ArrayOfVector& argIn);
//=============================================================================
ArrayOfVector
assert_columnVectorBuiltin(int nLhs, const ArrayOfVector& argIn);
//=============================================================================
ArrayOfVector
assert_squareMatrixBuiltin(int nLhs, const ArrayOfVector& argIn);
//=============================================================================
ArrayOfVector
assert_ndimsBuiltin(int nLhs, const ArrayOfVector& argIn);
//=============================================================================
ArrayOfVector
assert_lengthBuiltin(int nLhs, const ArrayOfVector& argIn);
//=============================================================================
ArrayOfVector
assert_rowsBuiltin(int nLhs, const ArrayOfVector& argIn);
//=============================================================================
ArrayOfVector
assert_columnsBuiltin(int nLhs, const ArrayOfVector& argIn);
//=============================================================================
ArrayOfVector
assert_startsWithBuiltin(int nLhs, const ArrayOfVector& argIn);
//=============================================================================
ArrayOfVector
assert_endsWithBuiltin(int nLhs, const ArrayOfVector& argIn);
//=============================================================================
ArrayOfVector
assert_containsAllBuiltin(int nLhs, const ArrayOfVector& argIn);
//=============================================================================
ArrayOfVector
assert_containsAnyBuiltin(int nLhs, const ArrayOfVector& argIn);
//=============================================================================
ArrayOfVector
assert_matchesAllBuiltin(int nLhs, const ArrayOfVector& argIn);
//=============================================================================
ArrayOfVector
assert_matchesAnyBuiltin(int nLhs, const ArrayOfVector& argIn);
//=============================================================================
ArrayOfVector
assert_hasFieldBuiltin(int nLhs, const ArrayOfVector& argIn);
//=============================================================================
ArrayOfVector
assert_hasFieldsBuiltin(int nLhs, const ArrayOfVector& argIn);
//=============================================================================
ArrayOfVector
assert_fieldsBuiltin(int nLhs, const ArrayOfVector& argIn);
//=============================================================================
ArrayOfVector
assert_satisfiesBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
//=============================================================================
} // namespace Nelson::AssertFunctionsGateway
//=============================================================================
