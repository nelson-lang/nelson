//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "getavailablelanguagesBuiltin.hpp"
#include "Localization.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::LocalizationGateway::getavailablelanguagesBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    wstringVector langs;
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 0, 0);
    Localization::Instance()->getManagedLanguages(langs);
    retval << ArrayOf::toCellArrayOfCharacterColumnVectors(langs);
    return retval;
}
//=============================================================================
