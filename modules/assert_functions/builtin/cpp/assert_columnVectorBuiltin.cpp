//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "assert_shapeHelpers.hpp"
#include "ErrorID.hpp"
//=============================================================================
ArrayOfVector
Nelson::AssertFunctionsGateway::assert_columnVectorBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 2);
    bool passed = argIn[0].isColumnVector();
    std::wstring msg = passed
        ? L""
        : formatI18nMessage(
              resolveMessageTemplate(L"Nelson:assert_functions:assertionFailedAssertsColumnvector",
                  L"Assertion failed: asserts.columnVector expected column vector, computed %s."),
              formatClassAndSize(argIn[0]));
    return assertionResult(nLhs, passed, msg);
}
//=============================================================================
