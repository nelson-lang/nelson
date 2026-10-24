//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "isprefBuiltin.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "PreferencesBuiltinHelpers.hpp"
#include "PreferencesStore.hpp"
//=============================================================================
namespace Nelson::PreferencesGateway {
//=============================================================================
ArrayOfVector
isprefBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 1, 2);
    ArrayOfVector retval;
    std::wstring group = Preferences::extractScalarName(argIn[0], L"group");
    if (argIn.size() == 1) {
        retval << ArrayOf::logicalScalar(Preferences::hasPreferenceGroup(group));
        return retval;
    }
    Preferences::PreferenceNames names
        = Preferences::extractPreferenceNames(argIn[1], L"preference");
    std::vector<bool> values = Preferences::hasPreferences(group, names);
    retval << (names.scalar ? ArrayOf::logicalScalar(values[0])
                            : makeLogicalArray(values, names.dimensions));
    return retval;
}
//=============================================================================
} // namespace Nelson::PreferencesGateway
//=============================================================================
