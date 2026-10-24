//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "rmprefBuiltin.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "PreferencesStore.hpp"
//=============================================================================
namespace Nelson::PreferencesGateway {
//=============================================================================
ArrayOfVector
rmprefBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargoutcheck(nLhs, 0, 0);
    nargincheck(argIn, 1, 2);
    std::wstring group = Preferences::extractScalarName(argIn[0], L"group");
    if (argIn.size() == 1) {
        Preferences::removePreferenceGroup(group);
    } else {
        Preferences::PreferenceNames names
            = Preferences::extractPreferenceNames(argIn[1], L"preference");
        Preferences::removePreferences(group, names);
    }
    return {};
}
//=============================================================================
} // namespace Nelson::PreferencesGateway
//=============================================================================
