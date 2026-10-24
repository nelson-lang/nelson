//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <vector>
#include <string>
#include <unordered_map>
#include <cmath>
#include <cstring>
#include "ismemberBuiltin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "OverloadRequired.hpp"
#include "IsMember.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
static bool
isRealComparableRows(const ArrayOf& A)
{
    return (A.isNumeric() || A.isLogical() || A.isCharacterArray()) && !A.isComplex()
        && !A.isSparse();
}
//=============================================================================
static ArrayOfVector
ismemberRows(int nLhs, const ArrayOf& A, const ArrayOf& B, bool& needToOverload)
{
    if (!isRealComparableRows(A) || !isRealComparableRows(B)) {
        needToOverload = true;
        return {};
    }
    Dimensions dimsA = A.dimensions();
    Dimensions dimsB = B.dimensions();
    if (!dimsA.is2D() || !dimsB.is2D()) {
        Error(L"Nelson:operators:rowsMode2DOnly", L"'rows' mode only works for 2D matrix.");
    }
    indexType cols = dimsA.columns();
    indexType mA = dimsA.rows();
    indexType nB = dimsB.rows();
    if (cols != dimsB.columns() && mA != 0 && nB != 0) {
        Error(L"Nelson:dimagree:inputsNumberColumns",
            L"Inputs must have the same number of columns.");
    }
    ArrayOf Ad(A);
    ArrayOf Bd(B);
    Ad.promoteType(NLS_DOUBLE);
    Bd.promoteType(NLS_DOUBLE);
    const double* pa = (const double*)Ad.dataPointer();
    const double* pb = (const double*)Bd.dataPointer();
    logical* tf = (logical*)ArrayOf::allocateArrayOf(NLS_LOGICAL, mA);
    bool wantLoc = (nLhs > 1);
    double* loc = wantLoc ? (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, mA) : nullptr;
    // Hash the rows of B once (first occurrence wins) and look each row of A up in O(1).
    // The key is the row's bytes with -0 normalised to +0 (so -0 and +0 match, as !=
    // does); a row containing NaN is never inserted or matched, since NaN != NaN.
    auto buildKey
        = [cols](const double* base, indexType row, indexType stride, std::string& key) -> bool {
        key.resize((size_t)cols * sizeof(double));
        for (indexType c = 0; c < cols; ++c) {
            double v = base[row + c * stride];
            if (std::isnan(v)) {
                return false;
            }
            if (v == 0.0) {
                v = 0.0;
            }
            std::memcpy(&key[(size_t)c * sizeof(double)], &v, sizeof(double));
        }
        return true;
    };
    std::unordered_map<std::string, indexType> firstIndexB;
    firstIndexB.reserve((size_t)nB * 2 + 1);
    std::string key;
    for (indexType j = 0; j < nB; ++j) {
        if (buildKey(pb, j, nB, key)) {
            firstIndexB.emplace(key, j + 1);
        }
    }
    for (indexType i = 0; i < mA; ++i) {
        tf[i] = 0;
        if (wantLoc) {
            loc[i] = 0;
        }
        if (!buildKey(pa, i, mA, key)) {
            continue;
        }
        auto it = firstIndexB.find(key);
        if (it != firstIndexB.end()) {
            tf[i] = 1;
            if (wantLoc) {
                loc[i] = (double)it->second;
            }
        }
    }
    ArrayOfVector retval;
    retval << ArrayOf(NLS_LOGICAL, Dimensions(mA, 1), tf);
    if (wantLoc) {
        retval << ArrayOf(NLS_DOUBLE, Dimensions(mA, 1), loc);
    }
    return retval;
}
//=============================================================================
ArrayOfVector
Nelson::OperatorsGateway::ismemberBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 2, 3);
    nargoutcheck(nLhs, 0, 2);

    bool needToOverload = false;
    if (argIn.size() == 3) {
        if (!(argIn[2].isRowVectorCharacterArray() || argIn[2].isScalarStringArray())) {
            Error(L"Nelson:operators:optionNotText",
                L"ismember option must be a character vector or string.");
        }
        std::wstring option = argIn[2].asWideString();
        if (option != L"rows") {
            Error(L"Nelson:operators:invalidOption", L"ismember option must be 'rows'.");
        }
        retval = ismemberRows(nLhs, argIn[0], argIn[1], needToOverload);
        if (needToOverload) {
            OverloadRequired("ismember");
        }
        return retval;
    }

    if (nLhs > 1) {
        ArrayOf locations;
        ArrayOf res = IsMember(argIn[0], argIn[1], locations, needToOverload);
        if (needToOverload) {
            OverloadRequired("ismember");
        } else {
            retval << res;
            retval << locations;
        }
    } else {
        ArrayOf res = IsMember(argIn[0], argIn[1], needToOverload);
        if (needToOverload) {
            OverloadRequired("ismember");
        } else {
            retval << res;
        }
    }
    return retval;
}
//=============================================================================
