//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include <string>
#include <vector>
#include "ArrayOf.hpp"
#include "nlsPreferences_exports.h"
//=============================================================================
namespace Nelson::Preferences {
//=============================================================================
struct PreferenceNames
{
    wstringVector names;
    Dimensions dimensions;
    bool scalar = false;
};
//=============================================================================
NLSPREFERENCES_IMPEXP std::wstring
preferencesFilePath();
NLSPREFERENCES_IMPEXP PreferenceNames
extractPreferenceNames(const ArrayOf& value, const std::wstring& argumentName);
NLSPREFERENCES_IMPEXP std::wstring
extractScalarName(const ArrayOf& value, const std::wstring& argumentName);
NLSPREFERENCES_IMPEXP ArrayOf
getAllPreferences();
NLSPREFERENCES_IMPEXP bool
hasPreferenceGroup(const std::wstring& group);
NLSPREFERENCES_IMPEXP std::vector<bool>
hasPreferences(const std::wstring& group, const PreferenceNames& preferences);
NLSPREFERENCES_IMPEXP ArrayOfVector
getPreferences(const std::wstring& group, const PreferenceNames& preferences);
NLSPREFERENCES_IMPEXP ArrayOf
getPreferenceOrDefault(
    const std::wstring& group, const std::wstring& preference, const ArrayOf& defaultValue);
NLSPREFERENCES_IMPEXP bool
getPreferenceIfExists(const std::wstring& group, const std::wstring& preference, ArrayOf& value);
NLSPREFERENCES_IMPEXP void
setPreferences(const std::wstring& group, const PreferenceNames& preferences,
    const ArrayOfVector& values, bool failIfExists);
NLSPREFERENCES_IMPEXP void
removePreferenceGroup(const std::wstring& group);
NLSPREFERENCES_IMPEXP void
removePreferences(const std::wstring& group, const PreferenceNames& preferences);
NLSPREFERENCES_IMPEXP void
registerUiPreference(const std::wstring& group, const std::wstring& preference);
NLSPREFERENCES_IMPEXP ArrayOf
resetRegisteredUiPreferences();
//=============================================================================
} // namespace Nelson::Preferences
//=============================================================================
