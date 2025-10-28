//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#define FMT_HEADER_ONLY
#include <fmt/printf.h>
#include <fmt/format.h>
#include <fmt/xchar.h>
#include "eyeBuiltin.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "Eye.hpp"
#include "StringToClass.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
static void
extractDestClassAndSparse(
    const ArrayOfVector& argIn, sizeType& nRhs, NelsonType& destClass, bool& bIsSparse)
{
    if (nRhs == 0) {
        return;
    }
    ArrayOf lastarg = argIn[nRhs - 1];
    if (lastarg.isRowVectorCharacterArray()) {
        destClass = StringToClass(lastarg.getContentAsCString());
        nRhs--;
        return;
    }
    if (nRhs >= 2) {
        ArrayOf secondLast = argIn[nRhs - 2];
        if (secondLast.isRowVectorCharacterArray() || secondLast.isScalarStringArray()) {
            std::wstring arg = secondLast.getContentAsWideString();
            if (arg == L"like") {
                bIsSparse = argIn[nRhs - 1].isSparse();
                destClass = argIn[nRhs - 1].getDataClass();
                nRhs -= 2;
                return;
            } else {
                const std::wstring fmt = std::wstring(ERROR_WRONG_ARGUMENT_X_VALUE);
                // report a 1-based argument index to the user
                int humanArgIndex = static_cast<int>(nRhs - 2) + 1;
                Error(fmt::sprintf(fmt, humanArgIndex));
            }
        }
    }
}
//=============================================================================
static void
parseDimensions(const ArrayOfVector& argIn, sizeType nRhs, indexType& n, indexType& m)
{
    if (nRhs == 0) {
        n = 1;
        m = 1;
        return;
    }
    if (nRhs == 1) {
        const ArrayOf& a = argIn[0];
        if (a.isScalar()) {
            n = a.getContentAsScalarIndex(true, true, true);
            m = n;
            return;
        } else if (a.isRowVector()) {
            if (a.getElementCount() == 2) {
                ArrayOf dimVector(a);
                indexType* pIndex = dimVector.getContentAsIndexPointer();
                n = pIndex[0];
                m = pIndex[1];
                delete[] pIndex;
                return;
            } else {
                Error(_W("N-dimensional arrays are not supported."));
            }
        } else {
            Error(_W("Size vector should be a row vector with real elements."));
        }
    } else if (nRhs == 2) {
        n = argIn[0].getContentAsScalarIndex(true, true, true);
        m = argIn[1].getContentAsScalarIndex(true, true, true);
        return;
    } else {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
}
//=============================================================================
ArrayOfVector
Nelson::ConstructorsGateway::eyeBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    bool bIsSparse = false;
    NelsonType destClass = NLS_DOUBLE;
    indexType n = 1;
    indexType m = 1;
    nargoutcheck(nLhs, 0, 1);
    ArrayOfVector retval(1);
    sizeType nRhs = argIn.size();

    // Extract optional destination class / like <array> syntax
    extractDestClassAndSparse(argIn, nRhs, destClass, bIsSparse);

    // Parse dimensions
    parseDimensions(argIn, nRhs, n, m);

    retval << Eye(n, m, destClass, bIsSparse);
    return retval;
}
//=============================================================================
