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
#include "ArrayOf.hpp"
#include "PreferencesStore.hpp"
//=============================================================================
namespace Nelson::PreferencesGateway {
//=============================================================================
ArrayOf
makeCellArray(const ArrayOfVector& values, const Dimensions& dimensions);
ArrayOf
makeLogicalArray(const std::vector<bool>& values, const Dimensions& dimensions);
ArrayOfVector
valuesArgumentToVector(
    const Nelson::Preferences::PreferenceNames& preferences, const ArrayOf& value);
std::wstring
lowerPreferenceText(const std::wstring& text);
bool
isAskPreferenceValue(const ArrayOf& value);
//=============================================================================
} // namespace Nelson::PreferencesGateway
//=============================================================================
