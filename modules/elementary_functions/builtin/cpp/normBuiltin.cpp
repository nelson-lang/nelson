//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "normBuiltin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "OverloadRequired.hpp"
#include <algorithm>
#include <cmath>
#include <cwctype>
#include <limits>
#include "Norm.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::ElementaryFunctionsGateway::normBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 2);
    nargoutcheck(nLhs, 0, 1);
    if (argIn[0].dataClass() == NLS_SINGLE || argIn[0].dataClass() == NLS_SCOMPLEX
        || argIn[0].dataClass() == NLS_DOUBLE || argIn[0].dataClass() == NLS_DCOMPLEX) {
        if (argIn.size() > 1) {
            if (argIn[1].isRowVectorCharacterArray() || argIn[1].isScalarStringArray()) {
                // 'fro' and 'inf' (any case, possibly abbreviated)
                std::wstring param = argIn[1].asWideString();
                std::transform(param.begin(), param.end(), param.begin(), ::towlower);
                if (!param.empty() && std::wstring(L"fro").compare(0, param.size(), param) == 0) {
                    retval << NormFrobenius(argIn[0]);
                } else if (!param.empty()
                    && std::wstring(L"inf").compare(0, param.size(), param) == 0) {
                    retval << Norm(argIn[0], std::numeric_limits<double>::infinity());
                } else {
                    Error(L"Nelson:invalidValue", L"Wrong value for #%d argument.", (2));
                }
            } else {
                ArrayOf param = argIn[1];
                if (param.isSparse()) {
                    param.makeDense();
                }
                if (!param.isNumeric() || !param.isScalar() || param.isComplex()) {
                    Error(L"Nelson:invalidValue", L"Wrong value for #%d argument.", (2));
                }
                double p = param.asDoubleScalar();
                if (std::isnan(p)) {
                    // the p-norm of a vector with p = NaN is NaN; no such matrix norm
                    if (!argIn[0].isVector()) {
                        Error(L"Nelson:invalidValue", L"Wrong value for #%d argument.", (2));
                    }
                    bool isSingle = (argIn[0].dataClass() == NLS_SINGLE
                        || argIn[0].dataClass() == NLS_SCOMPLEX);
                    retval << (isSingle ? ArrayOf::singleScalar(std::nanf(""))
                                        : ArrayOf::doubleScalar(std::nan("")));
                    return retval;
                }
                retval << Norm(argIn[0], p);
            }
        } else {
            retval << Norm(argIn[0], 2);
        }
    } else {
        Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (1),
            L"single or double");
    }

    return retval;
}
//=============================================================================
