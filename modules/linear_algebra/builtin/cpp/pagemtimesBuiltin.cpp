//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "pagemtimesBuiltin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "PageMatrixMultiply.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "OverloadRequired.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
static int
parseTranspose(const ArrayOf& arg)
{
    if (!arg.isRowVectorCharacterArray() && !(arg.isStringArray() && arg.isScalar())) {
        Error("Nelson:pagemtimes:invalidTransposeOption",
            "Transpose option must be 'none', 'transpose' or 'ctranspose'.");
    }
    std::wstring s = arg.asWideString();
    if (s == L"none") {
        return 0;
    }
    if (s == L"transpose") {
        return 1;
    }
    if (s == L"ctranspose") {
        return 2;
    }
    Error("Nelson:pagemtimes:invalidTransposeOption",
        "Transpose option must be 'none', 'transpose' or 'ctranspose'.");
    return 0;
}
//=============================================================================
ArrayOfVector
Nelson::LinearAlgebraGateway::pagemtimesBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);
    if (argIn.size() != 2 && argIn.size() != 4) {
        Error("Nelson:linear_algebra:pagemtimesWrongNumberOfInputs",
            "PAGEMTIMES requires 2 or 4 arguments.");
    }
    ArrayOf A;
    ArrayOf B;
    int transpA = 0;
    int transpB = 0;
    if (argIn.size() == 2) {
        A = argIn[0];
        B = argIn[1];
    } else {
        A = argIn[0];
        transpA = parseTranspose(argIn[1]);
        B = argIn[2];
        transpB = parseTranspose(argIn[3]);
    }
    bool needToOverload = false;
    ArrayOf res = PageMatrixMultiply(A, transpA, B, transpB, needToOverload);
    if (needToOverload) {
        OverloadRequired("pagemtimes");
    }
    retval << res;
    return retval;
}
//=============================================================================
