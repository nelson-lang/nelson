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
        retval << ArrayOf::doubleConstructor(Epsilon(1.0));
    } else {
        if (argIn[0].getDataClass() == NLS_DOUBLE || argIn[0].getDataClass() == NLS_DCOMPLEX) {
            if (!argIn[0].isScalar()) {
                Error(ERROR_WRONG_ARGUMENT_1_SIZE_SCALAR_EXPECTED);
            }
            auto* pV = (double*)argIn[0].getDataPointer();
            double dV = pV[0];
            retval << ArrayOf::doubleConstructor(Epsilon(dV));
        } else if (argIn[0].getDataClass() == NLS_SINGLE
            || argIn[0].getDataClass() == NLS_SCOMPLEX) {
            if (!argIn[0].isScalar()) {
                Error(ERROR_WRONG_ARGUMENT_1_SIZE_SCALAR_EXPECTED);
            }
            auto* pV = (single*)argIn[0].getDataPointer();
            single dV = pV[0];
            retval << ArrayOf::singleConstructor(Epsilon(dV));
        } else if (argIn[0].isRowVectorCharacterArray()) {
            std::wstring arg = argIn[0].getContentAsWideString();
            if (arg == L"single") {
                retval << ArrayOf::singleConstructor(Epsilon(static_cast<single>(1.0)));
            } else if (arg == L"double") {
                retval << ArrayOf::doubleConstructor(Epsilon(1.0));
            } else {
                Error(_W("Type \'double\' or \'single\' expected."));
            }
        } else {
            Error(_W("Type \'double\' or \'single\' expected."));
        }
    }
    return retval;
}
//=============================================================================
