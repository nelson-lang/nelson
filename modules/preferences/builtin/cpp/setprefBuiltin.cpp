//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "setprefBuiltin.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "PreferencesBuiltinHelpers.hpp"
#include "PreferencesStore.hpp"
//=============================================================================
namespace Nelson::PreferencesGateway {
//=============================================================================
ArrayOfVector
setprefBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargoutcheck(nLhs, 0, 0);
    nargincheck(argIn, 3, 3);
    std::wstring group = Preferences::extractScalarName(argIn[0], L"group");
    Preferences::PreferenceNames names
        = Preferences::extractPreferenceNames(argIn[1], L"preference");
    Preferences::setPreferences(group, names, valuesArgumentToVector(names, argIn[2]), false);
    return {};
}
//=============================================================================
} // namespace Nelson::PreferencesGateway
//=============================================================================
