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
#include "Types.hpp"
#include "nlsHelp_browser_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
class NLSHELP_BROWSER_IMPEXP HelpCollection
{
public:
    static HelpCollection*
    getInstance();
    bool
    registerHelpFiles(const wstringVector& filenames);
    bool
    unregisterHelpFiles(const wstringVector& filenames);

    wstringVector
    stripNonExistingHelpFiles();

    wstringVector
    getRegisteredFiles();

    bool
    clearCache();

    void
    destroy();

    std::wstring
    getNelsonCachedCollectionFullFilename();

    wstringVector
    searchByIdentifier(const std::wstring& identifier);
    wstringVector
    searchByName(const std::wstring& name);

private:
    static HelpCollection* m_pInstance;

    HelpCollection();

    std::wstring
    getNelsonCollectionFullFilename();

    std::wstring
    getNelsonCacheCollectionPath();
    std::wstring
    getNelsonQhcFilename();
};
//=============================================================================
} // namespace Nelson
//=============================================================================
