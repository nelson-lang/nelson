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
#include "nlsI18n_exports.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
class NLSI18N_IMPEXP TranslationManager
{
public:
    static TranslationManager&
    getInstance()
    {
        static TranslationManager instance;
        return instance;
    }

    TranslationManager(const TranslationManager&) = delete;
    TranslationManager&
    operator=(const TranslationManager&)
        = delete;

    bool
    loadModuleTranslation(
        const std::wstring& moduleName, const std::wstring& modulePath, const std::wstring& locale);

    std::wstring
    getMessage(const std::wstring& key) const;
    std::wstring
    getError(const std::wstring& key) const;
    std::string
    getMessage(const std::string& key) const;
    std::string
    getError(const std::string& key) const;

    void
    reset();

private:
    TranslationManager();
    ~TranslationManager();

    struct TranslationManagerData;
    TranslationManagerData* data;
};
//=============================================================================
}
//=============================================================================
