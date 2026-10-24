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
#include <vector>
#include "nlsI18n_exports.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
NLSI18N_IMPEXP void
forceRefreshLocale();
//=============================================================================
// Registers an external module's locale catalog so its _()/_W() strings and its
// "Nelson:<module>:<mnemonic>" error templates are localized. The module ships
// <moduleRoot>/locale/<moduleName>-ui-<locale>.json and
// <moduleName>-errors-<locale>.json, merged for the current locale and re-merged on
// every locale change. Call it from etc/startup.m (and unregisterModuleCatalog
// from etc/finish.m).
NLSI18N_IMPEXP void
registerModuleCatalog(const std::wstring& moduleName, const std::wstring& moduleRoot);
NLSI18N_IMPEXP void
unregisterModuleCatalog(const std::wstring& moduleName);
//=============================================================================
NLSI18N_IMPEXP std::wstring
gettextw(const std::wstring& S);
NLSI18N_IMPEXP std::wstring
gettextw(const std::string& S);
NLSI18N_IMPEXP std::string
gettext(const std::string& S);
//=============================================================================
// Resolve an error/warning message template from its identifier ("Nelson:module:mnemonic").
// Returns the template for the current locale (falling back to English), or an empty string
// when the identifier is not in the error catalog.
NLSI18N_IMPEXP std::wstring
getMessageTemplateById(const std::wstring& id);
NLSI18N_IMPEXP std::string
getMessageTemplateById(const std::string& id);
// The English (default-locale) template for an identifier, regardless of the current locale.
NLSI18N_IMPEXP std::wstring
getEnglishMessageTemplateById(const std::wstring& id);
//=============================================================================
namespace i18n_detail {
    //=============================================================================
    // First placeholder ("%s" or "%d") of a translated template. Both are two
    // characters wide, so replacing two characters covers either kind.
    inline size_t
    findFirstPlaceholder(const std::wstring& s)
    {
        const size_t ps = s.find(L"%s");
        const size_t pd = s.find(L"%d");
        if (ps == std::wstring::npos) {
            return pd;
        }
        if (pd == std::wstring::npos) {
            return ps;
        }
        return (ps < pd) ? ps : pd;
    }
    //=============================================================================
    inline size_t
    findFirstPlaceholder(const std::string& s)
    {
        const size_t ps = s.find("%s");
        const size_t pd = s.find("%d");
        if (ps == std::string::npos) {
            return pd;
        }
        if (pd == std::string::npos) {
            return ps;
        }
        return (ps < pd) ? ps : pd;
    }
    //=============================================================================
    // Textual form of one format argument: strings are substituted verbatim, integers
    // (argument indices) as decimal digits. A template such as
    // "Wrong type for argument #%d: %s expected." stays one catalog entry shared by
    // C++ and .m.
    inline std::wstring
    i18nArgToString(const std::wstring& v)
    {
        return v;
    }
    //=============================================================================
    inline std::wstring
    i18nArgToString(const wchar_t* v)
    {
        return std::wstring(v);
    }
    //=============================================================================
    inline std::wstring
    i18nArgToString(int v)
    {
        return std::to_wstring(v);
    }
    //=============================================================================
    inline std::wstring
    i18nArgToString(long v)
    {
        return std::to_wstring(v);
    }
    //=============================================================================
    inline std::wstring
    i18nArgToString(long long v)
    {
        return std::to_wstring(v);
    }
    //=============================================================================
    inline std::wstring
    i18nArgToString(unsigned int v)
    {
        return std::to_wstring(v);
    }
    //=============================================================================
    inline std::wstring
    i18nArgToString(unsigned long v)
    {
        return std::to_wstring(v);
    }
    //=============================================================================
    inline std::wstring
    i18nArgToString(unsigned long long v)
    {
        return std::to_wstring(v);
    }
    //=============================================================================
    inline std::string
    i18nArgToStringN(const std::string& v)
    {
        return v;
    }
    //=============================================================================
    inline std::string
    i18nArgToStringN(const char* v)
    {
        return std::string(v);
    }
    //=============================================================================
    inline std::string
    i18nArgToStringN(int v)
    {
        return std::to_string(v);
    }
    //=============================================================================
    inline std::string
    i18nArgToStringN(long v)
    {
        return std::to_string(v);
    }
    //=============================================================================
    inline std::string
    i18nArgToStringN(long long v)
    {
        return std::to_string(v);
    }
    //=============================================================================
    inline std::string
    i18nArgToStringN(unsigned int v)
    {
        return std::to_string(v);
    }
    //=============================================================================
    inline std::string
    i18nArgToStringN(unsigned long v)
    {
        return std::to_string(v);
    }
    //=============================================================================
    inline std::string
    i18nArgToStringN(unsigned long long v)
    {
        return std::to_string(v);
    }
    //=============================================================================
} // namespace i18n_detail
//=============================================================================
// Replaces the first placeholder ("%s" for a string, "%d" for an integer index) of
// a translated template by 'arg'. It is a plain string replacement, not a printf
// conversion, so wide arguments and arguments containing '%' are safe. Callers keep
// one translatable sentence such as _W("%s must be real numeric.") instead of
// concatenated fragments, which cannot be translated with a correct word order,
// and C++ and .m share the same catalog entry.
inline std::wstring
formatI18nMessage(const std::wstring& messageTemplate, const std::wstring& arg)
{
    std::wstring result = messageTemplate;
    const size_t position = i18n_detail::findFirstPlaceholder(result);
    if (position != std::wstring::npos) {
        result.replace(position, 2, arg);
    }
    return result;
}
//=============================================================================
inline std::string
formatI18nMessage(const std::string& messageTemplate, const std::string& arg)
{
    std::string result = messageTemplate;
    const size_t position = i18n_detail::findFirstPlaceholder(result);
    if (position != std::string::npos) {
        result.replace(position, 2, arg);
    }
    return result;
}
//=============================================================================
// Multi-argument variants: each placeholder ("%s" or "%d"), in order of appearance,
// takes the next argument. Strings and integers may be mixed; integers are rendered
// as decimal digits, e.g. formatI18nMessage(tmpl, index, typeName).
template <typename First, typename... Rest>
inline std::wstring
formatI18nMessage(const std::wstring& messageTemplate, const First& first, const Rest&... rest)
{
    std::wstring result = formatI18nMessage(messageTemplate, i18n_detail::i18nArgToString(first));
    if constexpr (sizeof...(rest) > 0) {
        result = formatI18nMessage(result, rest...);
    }
    return result;
}
//=============================================================================
template <typename First, typename... Rest>
inline std::string
formatI18nMessage(const std::string& messageTemplate, const First& first, const Rest&... rest)
{
    std::string result = formatI18nMessage(messageTemplate, i18n_detail::i18nArgToStringN(first));
    if constexpr (sizeof...(rest) > 0) {
        result = formatI18nMessage(result, rest...);
    }
    return result;
}
//=============================================================================
// Runtime variant: replaces the placeholders of a translated template, left to
// right, by arguments known only at run time (e.g. the Arguments of a message
// object), with the same plain replacement as the overloads above.
inline std::wstring
formatI18nMessageV(const std::wstring& messageTemplate, const std::vector<std::wstring>& args)
{
    std::wstring result = messageTemplate;
    for (const auto& arg : args) {
        const size_t position = i18n_detail::findFirstPlaceholder(result);
        if (position == std::wstring::npos) {
            break;
        }
        result.replace(position, 2, arg);
    }
    return result;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
#define _W(T) Nelson::gettextw(T)
#define _(T) Nelson::gettext(T)
//=============================================================================
