//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "ComputeNelsonPaths.hpp"
#include "ComputeNelsonRootPath.hpp"
#include "ComputeNelsonLibrariesPath.hpp"
#include "ComputeNelsonBinariesPath.hpp"
#include "ComputeNelsonModulesPath.hpp"
#include "ComputePreferencesPath.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
ComputesNelsonPaths(std::wstring& errorMessage)
{
    // Possible cases:
    // 1] GNU
    // ../prefix/bin/   --> binaries path
    // ../prefix/share/nelson --> nelson root path
    // ../prefix/share/nelson/modules --> nelson modules path
    // ../prefix/lib/nelson --> nelson libraries path
    //
    // 2] local build linux and/or Windows
    // ../nelson/bin/arch  --> binaries path
    // ../nelson --> nelson root path
    // ../nelson/modules --> nelson modules path
    // ../nelson/bin/arch  --> nelson libraries path

    // Order is important
    if (!ComputeNelsonLibrariesPath(errorMessage)) {
        return false;
    }
    if (!ComputeNelsonRootPath(errorMessage)) {
        return false;
    }
    if (!ComputeNelsonModulesPath(errorMessage)) {
        return false;
    }
    if (!ComputeNelsonBinariesPath(errorMessage)) {
        return false;
    }
    if (!ComputePreferencesPath(errorMessage)) {
        return false;
    }
    errorMessage.clear();
    return true;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
