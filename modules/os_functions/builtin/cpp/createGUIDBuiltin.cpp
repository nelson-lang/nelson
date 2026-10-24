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
#include "ErrorID.hpp"
#include "InputOutputArgumentsCheckers.hpp"
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
        retval << ArrayOf::characterArray(CreateGUID());
    } else {
        ArrayOf arg1 = argIn[0];
        if (arg1.isNumeric()) {
            indexType idx = arg1.asScalarIndex();
            wstringVector strs = CreateGUID(static_cast<size_t>(idx));
            retval << ArrayOf::cellArrayFromArrayOfCharacterColumnVectors(strs);
        } else {
            Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (1),
                L"double");
        }
    }
    return retval;
}
//=============================================================================
