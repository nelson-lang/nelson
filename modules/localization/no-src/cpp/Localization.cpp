//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "Localization.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
Localization* Localization::m_pInstance = nullptr;
//=============================================================================
Localization::Localization() { }
//=============================================================================
Localization*
Localization::Instance()
{
    if (m_pInstance == nullptr) {
        m_pInstance = new Localization();
    }
    return m_pInstance;
}
//=============================================================================
void
Localization::destroy()
{
    if (m_pInstance) {
        delete m_pInstance;
        m_pInstance = nullptr;
    }
}
//=============================================================================
std::wstring
Localization::getCurrentLanguage()
{
    return L"en_US";
}
//=============================================================================
std::wstring
Localization::getDefaultLanguage()
{
    return L"en_US";
}
//=============================================================================
bool
Localization::setLanguage(const std::wstring& lang, bool save)
{
    return lang == L"en_US";
}
//=============================================================================
bool
Localization::getManagedLanguages(wstringVector& langs)
{
    langs.clear();
    langs.push_back(L"en_US");
    return true;
}
//=============================================================================
std::wstring
Localization::initializeLocalization(const std::wstring& lang)
{
    return L"en_US";
}
//=============================================================================
bool
Localization::isSupportedLanguage(const std::wstring& lang)
{
    return wcscmp(lang.c_str(), L"en_US") == 0;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
