//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "createGUIDBuiltin.hpp"
#include "CreateGUID.hpp"
#include "Error.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "PredefinedErrorMessages.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::OsFunctionsGateway::createGUIDBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 0, 1);
    nargoutcheck(nLhs, 0, 1);
    ArrayOfVector retval;
    if (argIn.empty()) {
        retval << ArrayOf::characterArrayConstructor(CreateGUID());
    } else {
        ArrayOf arg1 = argIn[0];
        if (arg1.isNumeric()) {
            indexType idx = arg1.getContentAsScalarIndex();
            wstringVector strs = CreateGUID(static_cast<size_t>(idx));
            retval << ArrayOf::toCellArrayOfCharacterColumnVectors(strs);
        } else {
            Error(ERROR_WRONG_ARGUMENT_1_TYPE_DOUBLE_EXPECTED);
        }
    }
    return retval;
}
//=============================================================================
