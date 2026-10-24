//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <algorithm>
#include <cwctype>
#include "ifftBuiltin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "InverseFft.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "OverloadRequired.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
static ArrayOfVector
ifftBuiltinPrivate(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);
    ArrayOf res;
    nargincheck(argIn, 1, 4);
    ArrayOfVector args(argIn);
    bool asSymmetric = false;
    if (args.size() >= 2
        && (args[args.size() - 1].isRowVectorCharacterArray()
            || args[args.size() - 1].isScalarStringArray())) {
        std::wstring symflag = args[args.size() - 1].asWideString();
        std::transform(
            symflag.begin(), symflag.end(), symflag.begin(), [](wchar_t c) { return towlower(c); });
        if (symflag == L"symmetric") {
            asSymmetric = true;
        } else if (symflag != L"nonsymmetric") {
            Error(L"Nelson:ifft:invalidSymmetryFlag",
                L"Symmetry flag must be 'symmetric' or 'nonsymmetric'.");
        }
        args.pop_back();
    }
    nargincheck(args, 1, 3);
    ArrayOf X = args[0];
    switch (args.size()) {
    case 1: {
        // ifft(X)
        res = asSymmetric ? InverseFftSymmetric(X) : InverseFft(X);
    } break;
    case 2: {
        ArrayOf N = args[1];
        if (N.isNumeric() && N.isEmpty()) {
            // ifft(X, []);
            res = asSymmetric ? InverseFftSymmetric(X) : InverseFft(X);
        } else {
            // ifft(X, n)
            indexType n = N.asScalarIndex(true);
            res = asSymmetric ? InverseFftSymmetric(X, n) : InverseFft(X, n);
        }
    } break;
    case 3: {
        // ifft(X, n, dim)
        ArrayOf N = args[1];
        ArrayOf DIM = args[2];
        indexType n;
        indexType dim = DIM.asScalarIndex(false);
        if (N.isNumeric() && N.isEmpty()) {
            // ifft(X, [], dim)
            if (X.isScalar()) {
                n = 1;
            } else {
                n = X.dimensionLength(static_cast<int>(dim) - 1);
            }
        } else {
            n = N.asScalarIndex(true);
        }
        res = asSymmetric ? InverseFftSymmetric(X, n, dim - 1) : InverseFft(X, n, dim - 1);
    } break;
    }
    retval << res;
    return retval;
}
//=============================================================================
ArrayOfVector
Nelson::FftwGateway::ifftBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1);
    if (argIn[0].isSparse() || argIn[0].isCell() || argIn[0].isHandle() || argIn[0].isStruct()
        || argIn[0].isClassType()) {
        OverloadRequired("ifft");
    }
    retval = ifftBuiltinPrivate(nLhs, argIn);
    return retval;
}
//=============================================================================
