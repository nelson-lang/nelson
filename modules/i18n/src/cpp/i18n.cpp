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
} // namespace Nelson
//=============================================================================
