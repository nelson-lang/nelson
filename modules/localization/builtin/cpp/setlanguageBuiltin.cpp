//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "setlanguageBuiltin.hpp"
#include "Localization.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::LocalizationGateway::setlanguageBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 1, 1);
    std::wstring desiredLang = argIn[0].asWideString();
    if (Localization::Instance()->isSupportedLanguage(desiredLang)) {
        retval << ArrayOf::logicalScalar(Localization::Instance()->setLanguage(desiredLang, true));
    } else {
        retval << ArrayOf::logicalScalar(false);
    }
    return retval;
}
//=============================================================================
