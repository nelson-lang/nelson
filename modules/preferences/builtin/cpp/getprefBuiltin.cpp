//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "getprefBuiltin.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "PreferencesBuiltinHelpers.hpp"
#include "PreferencesStore.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson::PreferencesGateway {
//=============================================================================
ArrayOfVector
getprefBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 0, 3);
    ArrayOfVector retval;
    if (argIn.empty()) {
        retval << Preferences::getAllPreferences();
        return retval;
    }
    nargincheck(argIn, 2, 3);
    std::wstring group = Preferences::extractScalarName(argIn[0], L"group");
    Preferences::PreferenceNames names
        = Preferences::extractPreferenceNames(argIn[1], L"preference");
    if (argIn.size() == 2) {
        ArrayOfVector values = Preferences::getPreferences(group, names);
        retval << (names.scalar ? values[0] : makeCellArray(values, names.dimensions));
        return retval;
    }
    if (names.scalar) {
        retval << Preferences::getPreferenceOrDefault(group, names.names[0], argIn[2]);
        return retval;
    }
    ArrayOfVector defaults = valuesArgumentToVector(names, argIn[2]);
    ArrayOfVector values;
    values.reserve(names.names.size());
    for (size_t k = 0; k < names.names.size(); ++k) {
        values << Preferences::getPreferenceOrDefault(group, names.names[k], defaults[k]);
    }
    retval << makeCellArray(values, names.dimensions);
    return retval;
}
//=============================================================================
} // namespace Nelson::PreferencesGateway
//=============================================================================
