//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "i18n.hpp"
#include "characters_encoding.hpp"
#include "I18nManager.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
void
forceRefreshLocale()
{
    I18nManager::getInstance()->forceToUpdateLocaleData();
}
//=============================================================================
void
registerModuleCatalog(const std::wstring& moduleName, const std::wstring& moduleRoot)
{
    I18nManager::getInstance()->registerModuleCatalog(moduleName, moduleRoot);
}
//=============================================================================
void
unregisterModuleCatalog(const std::wstring& moduleName)
{
    I18nManager::getInstance()->unregisterModuleCatalog(moduleName);
}
//=============================================================================
std::wstring
gettextw(const std::wstring& S)
{
    return I18nManager::getInstance()->getTextW(S);
}
//=============================================================================
std::wstring
gettextw(const std::string& S)
{
    return I18nManager::getInstance()->getTextW(S);
}
//=============================================================================
std::string
gettext(const std::string& S)
{
    return I18nManager::getInstance()->getText(S);
}
//=============================================================================
std::wstring
getMessageTemplateById(const std::wstring& id)
{
    return I18nManager::getInstance()->getTemplateById(id);
}
//=============================================================================
std::wstring
getEnglishMessageTemplateById(const std::wstring& id)
{
    return I18nManager::getInstance()->getEnglishTemplateById(id);
}
//=============================================================================
std::string
getMessageTemplateById(const std::string& id)
{
    std::wstring resolved = I18nManager::getInstance()->getTemplateById(utf8_to_wstring(id));
    if (resolved.empty()) {
        return "";
    }
    return wstring_to_utf8(resolved);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
