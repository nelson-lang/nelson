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
#include <utility>
#include "Error.hpp"
#include "Warning.hpp"
#include "i18n.hpp"
//=============================================================================
// Identifier-keyed error/warning helpers.
//
//   Error(L"Nelson:module:mnemonic", L"english template %s", arg);
//   Warning(L"Nelson:module:mnemonic", L"english template");
//
// The message template stays inline at the call site (readable, and used as the English
// source harvested by the i18n extractor). At run time the message is resolved by a cascade:
//   1. the identifier-keyed error catalog (getMessageTemplateById), localized and stable;
//   2. otherwise the text-keyed UI catalog (gettextw of the inline template) - this keeps
//      overloaded identifiers (dropped from the identifier catalog) translated;
//   3. otherwise the inline English template itself.
// The resolved template is then formatted with the %s/%d arguments and thrown with the id.
//
// For a message built at run time (no fixed template), use the low-level RaiseError/
// RaiseWarning (see Error.hpp / Warning.hpp) instead. Error(msg) / Warning(msg) (a single
// argument) throw/emit a message with no identifier.
//=============================================================================
namespace Nelson {
//=============================================================================
inline std::wstring
resolveMessageTemplate(const std::wstring& id, const std::wstring& messageTemplate)
{
    std::wstring resolved = getMessageTemplateById(id);
    if (resolved.empty()) {
        resolved = gettextw(messageTemplate);
    }
    return resolved;
}
//=============================================================================
inline std::string
resolveMessageTemplate(const std::string& id, const std::string& messageTemplate)
{
    std::string resolved = getMessageTemplateById(id);
    if (resolved.empty()) {
        resolved = gettext(messageTemplate);
    }
    return resolved;
}
//=============================================================================
template <typename... Args>
inline void
Error(const std::wstring& id, const std::wstring& messageTemplate, Args&&... args)
{
    std::wstring resolved = resolveMessageTemplate(id, messageTemplate);
    if constexpr (sizeof...(args) > 0) {
        RaiseError(formatI18nMessage(resolved, std::forward<Args>(args)...), id);
    } else {
        RaiseError(resolved, id);
    }
}
//=============================================================================
template <typename... Args>
inline void
Error(const std::string& id, const std::string& messageTemplate, Args&&... args)
{
    std::string resolved = resolveMessageTemplate(id, messageTemplate);
    if constexpr (sizeof...(args) > 0) {
        RaiseError(formatI18nMessage(resolved, std::forward<Args>(args)...), id);
    } else {
        RaiseError(resolved, id);
    }
}
//=============================================================================
// Same cascade as Error but reports the error at the CALLER (asCaller = true).
// Used by argument checkers and assertions so the error points at the user's code.
template <typename... Args>
inline void
ErrorAsCaller(const std::wstring& id, const std::wstring& messageTemplate, Args&&... args)
{
    std::wstring resolved = resolveMessageTemplate(id, messageTemplate);
    if constexpr (sizeof...(args) > 0) {
        RaiseError(formatI18nMessage(resolved, std::forward<Args>(args)...), id, true);
    } else {
        RaiseError(resolved, id, true);
    }
}
//=============================================================================
template <typename... Args>
inline void
ErrorAsCaller(const std::string& id, const std::string& messageTemplate, Args&&... args)
{
    std::string resolved = resolveMessageTemplate(id, messageTemplate);
    if constexpr (sizeof...(args) > 0) {
        RaiseError(formatI18nMessage(resolved, std::forward<Args>(args)...), id, true);
    } else {
        RaiseError(resolved, id, true);
    }
}
//=============================================================================
template <typename... Args>
inline void
Warning(const std::wstring& id, const std::wstring& messageTemplate, Args&&... args)
{
    std::wstring resolved = resolveMessageTemplate(id, messageTemplate);
    if constexpr (sizeof...(args) > 0) {
        RaiseWarning(id, formatI18nMessage(resolved, std::forward<Args>(args)...));
    } else {
        RaiseWarning(id, resolved);
    }
}
//=============================================================================
template <typename... Args>
inline void
Warning(const std::string& id, const std::string& messageTemplate, Args&&... args)
{
    std::string resolved = resolveMessageTemplate(id, messageTemplate);
    if constexpr (sizeof...(args) > 0) {
        RaiseWarning(id, formatI18nMessage(resolved, std::forward<Args>(args)...));
    } else {
        RaiseWarning(id, resolved);
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
