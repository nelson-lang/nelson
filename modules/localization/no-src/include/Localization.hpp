//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include <string>
#include "Types.hpp"
#include "nlsLocalization_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
class NLSLOCALIZATION_IMPEXP Localization
{
public:
    static Localization*
    Instance();
    std::wstring
    getCurrentLanguage();
    std::wstring
    getDefaultLanguage();
    bool
    setLanguage(const std::wstring& lang, bool save = true);
    bool
    getManagedLanguages(wstringVector& langs);
    std::wstring
    initializeLocalization(const std::wstring& lang);
    bool
    isSupportedLanguage(const std::wstring& lang);
    void
    destroy();

private:
    Localization();
    Localization(Localization const& /*unused*/) {};
    static Localization* m_pInstance;
};
//=============================================================================
} // namespace Nelson
//=============================================================================
