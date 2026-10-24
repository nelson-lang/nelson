//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#ifdef _MSC_VER
#pragma warning(disable : 4190)
#endif
//=============================================================================
#include "NelsonGateway.hpp"
#include "assert_checkerrorBuiltin.hpp"
#include "assert_extraBuiltins.hpp"
#include "assert_isapproxBuiltin.hpp"
#include "assert_isequalBuiltin.hpp"
#include "assert_isfalseBuiltin.hpp"
#include "assert_istrueBuiltin.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"assert_functions";
//=============================================================================
#define ASSERTS_BUILTIN(NAME, FUNCTION, NOUT, NIN)                                                 \
    { "asserts." NAME, (ptrBuiltin)Nelson::AssertFunctionsGateway::FUNCTION, NOUT, NIN },          \
    {                                                                                              \
        "__asserts_" NAME "__", (ptrBuiltin)Nelson::AssertFunctionsGateway::FUNCTION, NOUT, NIN    \
    }
//=============================================================================
#define ASSERTS_BUILTIN_WITH_FLAG(NAME, FUNCTION, NOUT, NIN, FLAG)                                 \
    { "asserts." NAME, (ptrBuiltin)Nelson::AssertFunctionsGateway::FUNCTION, NOUT, NIN, FLAG },    \
    {                                                                                              \
        "__asserts_" NAME "__", (ptrBuiltin)Nelson::AssertFunctionsGateway::FUNCTION, NOUT, NIN,   \
            FLAG                                                                                   \
    }
//=============================================================================
static const nlsGateway gateway[] = {
    { "assert_istrue", (ptrBuiltin)Nelson::AssertFunctionsGateway::assert_istrueBuiltin, 2, -1 },
    ASSERTS_BUILTIN("istrue", assert_istrueBuiltin, 2, -1),
    { "assert_isfalse", (ptrBuiltin)Nelson::AssertFunctionsGateway::assert_isfalseBuiltin, 2, -1 },
    ASSERTS_BUILTIN("isfalse", assert_isfalseBuiltin, 2, -1),
    { "assert", (ptrBuiltin)Nelson::AssertFunctionsGateway::assertBuiltin, 2, -1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "assert_checkerror", (ptrBuiltin)Nelson::AssertFunctionsGateway::assert_checkerrorBuiltin, 2,
        -2, CPP_BUILTIN_WITH_EVALUATOR },
    ASSERTS_BUILTIN_WITH_FLAG(
        "checkerror", assert_checkerrorBuiltin, 2, -2, CPP_BUILTIN_WITH_EVALUATOR),
    { "assert_isequal", (ptrBuiltin)Nelson::AssertFunctionsGateway::assert_isequalBuiltin, 2, -2,
        CPP_BUILTIN_WITH_EVALUATOR },
    ASSERTS_BUILTIN_WITH_FLAG("isequal", assert_isequalBuiltin, 2, -2, CPP_BUILTIN_WITH_EVALUATOR),
    { "assert_isapprox", (ptrBuiltin)Nelson::AssertFunctionsGateway::assert_isapproxBuiltin, 2, -2,
        CPP_BUILTIN_WITH_EVALUATOR },
    ASSERTS_BUILTIN_WITH_FLAG(
        "isapprox", assert_isapproxBuiltin, 2, -2, CPP_BUILTIN_WITH_EVALUATOR),
    ASSERTS_BUILTIN("size", assert_sizeBuiltin, 2, 2),
    ASSERTS_BUILTIN("class", assert_classBuiltin, 2, 2),
    ASSERTS_BUILTIN("empty", assert_emptyBuiltin, 2, 1),
    ASSERTS_BUILTIN("notempty", assert_notemptyBuiltin, 2, 1),
    ASSERTS_BUILTIN("alltrue", assert_alltrueBuiltin, 2, 1),
    ASSERTS_BUILTIN("allfalse", assert_allfalseBuiltin, 2, 1),
    ASSERTS_BUILTIN("contains", assert_containsBuiltin, 2, 2),
    ASSERTS_BUILTIN("match", assert_matchBuiltin, 2, 2),
    ASSERTS_BUILTIN("type", assert_typeBuiltin, 2, 2),
    ASSERTS_BUILTIN("scalar", assert_scalarBuiltin, 2, 1),
    ASSERTS_BUILTIN("vector", assert_vectorBuiltin, 2, 1),
    ASSERTS_BUILTIN("matrix", assert_matrixBuiltin, 2, 1),
    ASSERTS_BUILTIN("real", assert_realBuiltin, 2, 1),
    ASSERTS_BUILTIN("finite", assert_finiteBuiltin, 2, 1),
    ASSERTS_BUILTIN("nonNan", assert_nonNanBuiltin, 2, 1),
    ASSERTS_BUILTIN("sameSize", assert_sameSizeBuiltin, 2, 2),
    ASSERTS_BUILTIN("numel", assert_numelBuiltin, 2, 2),
    ASSERTS_BUILTIN_WITH_FLAG("throws", assert_throwsBuiltin, 2, -2, CPP_BUILTIN_WITH_EVALUATOR),
    ASSERTS_BUILTIN_WITH_FLAG("warning", assert_warningBuiltin, 2, -2, CPP_BUILTIN_WITH_EVALUATOR),
    ASSERTS_BUILTIN_WITH_FLAG(
        "warningFree", assert_warningFreeBuiltin, 2, 1, CPP_BUILTIN_WITH_EVALUATOR),
    ASSERTS_BUILTIN_WITH_FLAG("diff", assert_diffBuiltin, 2, 2, CPP_BUILTIN_WITH_EVALUATOR),
    ASSERTS_BUILTIN("fail", assert_failBuiltin, 2, -1),
    ASSERTS_BUILTIN_WITH_FLAG("noError", assert_noErrorBuiltin, 2, 1, CPP_BUILTIN_WITH_EVALUATOR),
    ASSERTS_BUILTIN_WITH_FLAG("notEqual", assert_notEqualBuiltin, 2, 2, CPP_BUILTIN_WITH_EVALUATOR),
    ASSERTS_BUILTIN_WITH_FLAG(
        "notApprox", assert_notApproxBuiltin, 2, -3, CPP_BUILTIN_WITH_EVALUATOR),
    ASSERTS_BUILTIN("lessThan", assert_lessThanBuiltin, 2, 2),
    ASSERTS_BUILTIN("lessOrEqual", assert_lessOrEqualBuiltin, 2, 2),
    ASSERTS_BUILTIN("greaterThan", assert_greaterThanBuiltin, 2, 2),
    ASSERTS_BUILTIN("greaterOrEqual", assert_greaterOrEqualBuiltin, 2, 2),
    ASSERTS_BUILTIN("inRange", assert_inRangeBuiltin, 2, 3),
    ASSERTS_BUILTIN("rowVector", assert_rowVectorBuiltin, 2, 1),
    ASSERTS_BUILTIN("columnVector", assert_columnVectorBuiltin, 2, 1),
    ASSERTS_BUILTIN("squareMatrix", assert_squareMatrixBuiltin, 2, 1),
    ASSERTS_BUILTIN("ndims", assert_ndimsBuiltin, 2, 2),
    ASSERTS_BUILTIN("length", assert_lengthBuiltin, 2, 2),
    ASSERTS_BUILTIN("rows", assert_rowsBuiltin, 2, 2),
    ASSERTS_BUILTIN("columns", assert_columnsBuiltin, 2, 2),
    ASSERTS_BUILTIN("startsWith", assert_startsWithBuiltin, 2, 2),
    ASSERTS_BUILTIN("endsWith", assert_endsWithBuiltin, 2, 2),
    ASSERTS_BUILTIN("containsAll", assert_containsAllBuiltin, 2, 2),
    ASSERTS_BUILTIN("containsAny", assert_containsAnyBuiltin, 2, 2),
    ASSERTS_BUILTIN("matchesAll", assert_matchesAllBuiltin, 2, 2),
    ASSERTS_BUILTIN("matchesAny", assert_matchesAnyBuiltin, 2, 2),
    ASSERTS_BUILTIN("hasField", assert_hasFieldBuiltin, 2, 2),
    ASSERTS_BUILTIN("hasFields", assert_hasFieldsBuiltin, 2, 2),
    ASSERTS_BUILTIN("fields", assert_fieldsBuiltin, 2, 2),
    ASSERTS_BUILTIN_WITH_FLAG(
        "satisfies", assert_satisfiesBuiltin, 2, 2, CPP_BUILTIN_WITH_EVALUATOR),
};
//=============================================================================
NLS_GATEWAY_FUNC(gateway)
//=============================================================================
NLS_GATEWAY_REMOVE(gateway)
//=============================================================================
