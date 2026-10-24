//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#define _CRT_SECURE_NO_WARNINGS
#include "NFlowHelpers.hpp"
#include "ModulePath.hpp"
#include "characters_encoding.hpp"
#include "NelsonConfiguration.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
std::wstring
getNFlowDistributionDirectory()
{
    return ModulePath(L"nflow_gui", GET_ROOT_PATH) + L"/dist";
}
//=============================================================================
std::wstring
getNFlowPreferencesDirectory()
{

    return NelsonConfiguration::getInstance()->getNelsonPreferencesDirectory();
}
//=============================================================================
} // namespace Nelson
//=============================================================================
