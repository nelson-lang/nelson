//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "uisetprefBuiltin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "PreferencesStore.hpp"
#include "StringHelpers.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson::PreferencesGateway {
//=============================================================================
ArrayOfVector
uisetprefBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 1, 1);
    if (!StringHelpers::iequals(argIn[0].asWideString(), L"clearall")) {
        Error(L"Nelson:uisetpref:unknownCommand", L"Unknown preference command.");
    }
    ArrayOfVector retval;
    ArrayOf values = Preferences::resetRegisteredUiPreferences();
    if (nLhs > 0) {
        retval << values;
    }
    return retval;
}
//=============================================================================
} // namespace Nelson::PreferencesGateway
//=============================================================================
