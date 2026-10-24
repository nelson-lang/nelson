//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "epsBuiltin.hpp"
#include "Epsilon.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::ConstructorsGateway::epsBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 0, 1);
    ArrayOfVector retval(1);
    if (argIn.empty()) {
        retval << ArrayOf::doubleScalar(Epsilon(1.0));
    } else {
        if (argIn[0].dataClass() == NLS_DOUBLE || argIn[0].dataClass() == NLS_DCOMPLEX) {
            if (!argIn[0].isScalar()) {
                Error(L"Nelson:scalarExpected", L"Wrong size for argument #%d: scalar expected.",
                    (1));
            }
            auto* pV = (double*)argIn[0].dataPointer();
            double dV = pV[0];
            retval << ArrayOf::doubleScalar(Epsilon(dV));
        } else if (argIn[0].dataClass() == NLS_SINGLE || argIn[0].dataClass() == NLS_SCOMPLEX) {
            if (!argIn[0].isScalar()) {
                Error(L"Nelson:scalarExpected", L"Wrong size for argument #%d: scalar expected.",
                    (1));
            }
            auto* pV = (single*)argIn[0].dataPointer();
            single dV = pV[0];
            retval << ArrayOf::singleScalar(Epsilon(dV));
        } else if (argIn[0].isRowVectorCharacterArray()) {
            std::wstring arg = argIn[0].asWideString();
            if (arg == L"single") {
                retval << ArrayOf::singleScalar(Epsilon(static_cast<single>(1.0)));
            } else if (arg == L"double") {
                retval << ArrayOf::doubleScalar(Epsilon(1.0));
            } else {
                Error(L"Nelson:constructors_functions:typeDoubleSingleExpected",
                    L"Type \'double\' or \'single\' expected.");
            }
        } else {
            Error(L"Nelson:constructors_functions:typeDoubleSingleExpected",
                L"Type \'double\' or \'single\' expected.");
        }
    }
    return retval;
}
//=============================================================================
