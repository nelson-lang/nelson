//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "gcdBuiltin.hpp"
#include "GCD.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "FindCommonType.hpp"
#include "OverloadHelpers.hpp"
#include "OverloadRequired.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
// Extended Euclidean algorithm returning the Bezout coefficients x, y such that
// x*a + y*b = g, with g normalised to be non-negative (established convention).
static void
extendedGcd(long long a, long long b, long long& g, long long& x, long long& y)
{
    if (a == 0 && b == 0) {
        g = 0;
        x = 0;
        y = 0;
        return;
    }
    long long oldR = a, r = b;
    long long oldS = 1, s = 0;
    long long oldT = 0, t = 1;
    while (r != 0) {
        long long q = oldR / r;
        long long tmp = oldR - q * r;
        oldR = r;
        r = tmp;
        tmp = oldS - q * s;
        oldS = s;
        s = tmp;
        tmp = oldT - q * t;
        oldT = t;
        t = tmp;
    }
    g = oldR;
    x = oldS;
    y = oldT;
    if (g < 0) {
        g = -g;
        x = -x;
        y = -y;
    }
}
//=============================================================================
static void
computeBezoutCoefficients(
    const ArrayOf& A, const ArrayOf& B, const ArrayOf& G, ArrayOf& C, ArrayOf& D)
{
    ArrayOf Ad(A);
    ArrayOf Bd(B);
    Ad.promoteType(NLS_DOUBLE);
    Bd.promoteType(NLS_DOUBLE);
    Dimensions dims = G.dimensions();
    indexType N = dims.elementCount();
    double* pC = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, N);
    double* pD = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, N);
    const double* pa = (const double*)Ad.dataPointer();
    const double* pb = (const double*)Bd.dataPointer();
    bool aScalar = Ad.isScalar();
    bool bScalar = Bd.isScalar();
    for (indexType k = 0; k < N; ++k) {
        long long a = (long long)pa[aScalar ? 0 : k];
        long long b = (long long)pb[bScalar ? 0 : k];
        long long g = 0, x = 0, y = 0;
        extendedGcd(a, b, g, x, y);
        pC[k] = (double)x;
        pD[k] = (double)y;
    }
    C = ArrayOf(NLS_DOUBLE, dims, pC);
    D = ArrayOf(NLS_DOUBLE, dims, pD);
    NelsonType outClass = G.dataClass();
    if (outClass != NLS_DOUBLE) {
        C.promoteType(outClass);
        D.promoteType(outClass);
    }
}
//=============================================================================
ArrayOfVector
Nelson::SpecialFunctionsGateway::gcdBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    const std::string functionName = "gcd";
    std::string commonTypeName = NLS_UNKNOWN_STR;
    NelsonType commonType = NLS_UNKNOWN;
    bool isSparse = false;
    bool isComplex = false;
    nargincheck(argIn, 2, 2);
    nargoutcheck(nLhs, 0, 3);

    ArrayOf res;
    if (FindCommonType(argIn, commonType, isSparse, isComplex, commonTypeName)) {
        bool overloadWasFound = false;
        res = callOverloadedFunction(eval,
            NelsonConfiguration::getInstance()->getOverloadLevelCompatibility(), argIn,
            functionName, commonTypeName, commonType, overloadWasFound);
        if (overloadWasFound) {
            return res;
        }
    }

    bool needToOverload = false;
    ArrayOf A(argIn[0]);
    ArrayOf B(argIn[1]);
    if (A.dataClass() != B.dataClass()) {
        if (A.isIntegerType()) {
            bool isCompatible = (B.dataClass() == NLS_DOUBLE) && B.isScalar();
            if (!isCompatible) {
                Error(L"Nelson:gcd:integerClassMismatch",
                    L"Integers can only be combined with integers of the same class, or scalar "
                    L"doubles.");
            }
            A.promoteType(commonType);
            B.promoteType(commonType);
        } else if (B.isIntegerType()) {
            bool isCompatible = (A.dataClass() == NLS_DOUBLE) && A.isScalar();
            if (!isCompatible) {
                Error(L"Nelson:gcd:integerClassMismatch",
                    L"Integers can only be combined with integers of the same class, or scalar "
                    L"doubles.");
            }
            A.promoteType(commonType);
            B.promoteType(commonType);
        } else {
            if (commonType <= NLS_CHAR) {
                NelsonType _commonType = commonType;
                if (_commonType == NLS_CHAR) {
                    _commonType = NLS_DOUBLE;
                }
                if (_commonType == NLS_DOUBLE && isComplex) {
                    _commonType = NLS_DCOMPLEX;
                }
                if (_commonType == NLS_SINGLE && isComplex) {
                    _commonType = NLS_SCOMPLEX;
                }
                A.promoteType(_commonType);
                B.promoteType(_commonType);
            }
        }
    }
    res = GCD(A, B, needToOverload);

    if (needToOverload) {
        bool overloadWasFound = false;
        res = callOverloadedFunction(eval, NLS_OVERLOAD_ALL_TYPES, argIn, functionName,
            commonTypeName, commonType, overloadWasFound);
        if (!overloadWasFound) {
            OverloadRequired(functionName);
        }
        return res;
    }

    ArrayOfVector retval;
    retval << res;
    if (nLhs >= 2) {
        NelsonType outClass = res.dataClass();
        if (outClass == NLS_UINT8 || outClass == NLS_UINT16 || outClass == NLS_UINT32
            || outClass == NLS_UINT64) {
            // Bezout coefficients can be negative, so they cannot be returned in
            // an unsigned class.
            Error(L"Nelson:gcd:unsignedNotSupported",
                L"Unsigned integers are not supported for the three output syntax of GCD.");
        }
        // [G, C, D] = gcd(A, B): Bezout coefficients such that C.*A + D.*B = G.
        ArrayOf C, D;
        computeBezoutCoefficients(A, B, res, C, D);
        retval << C;
        if (nLhs >= 3) {
            retval << D;
        }
    }
    return retval;
}
//=============================================================================
