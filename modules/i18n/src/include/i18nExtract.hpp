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
#include <set>
#include <string>
#include "nlsI18n_exports.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
// Native i18n catalog extraction (no gettext tooling involved).
//
// i18nExtractUi scans the C/C++ and .m sources under sourceRoot (tests
// directories excluded) for the text-keyed helpers _(...), _W(...) and TR(...)
// and writes the user-interface catalog (key = source text, value = empty so
// the runtime falls back to the key) to jsonFile.
NLSI18N_IMPEXP bool
i18nExtractUi(
    const std::wstring& sourceRoot, const std::wstring& jsonFile, std::wstring& errorMessage);
//=============================================================================
// i18nExtractErrors scans the same sources for identifier-keyed error/warning
// templates: Error / ErrorAsCaller / Warning / resolveMessageTemplate with a
// literal identifier and a literal template in C/C++, and
// error('Nelson:...', 'tmpl') / warning('Nelson:...', 'tmpl') / nmm_error in
// .m. It writes the errors catalog (key = identifier, value = English
// template) to jsonFile. An identifier mapped to several distinct templates
// ("overloaded") is dropped from the catalog and reported on stdout.
NLSI18N_IMPEXP bool
i18nExtractErrors(
    const std::wstring& sourceRoot, const std::wstring& jsonFile, std::wstring& errorMessage);
//=============================================================================
// Catalog maintenance collectors (used by the noindex and dedup passes).
//
// Identifiers used somewhere with a run-time (non-literal) cascade message in
// Error(id, X) / ErrorAsCaller(id, X) / Warning(id, X). C/C++ sources, tests
// directories excluded.
NLSI18N_IMPEXP std::set<std::string>
collectRuntimeMessageIds(const std::wstring& sourceRoot);
//=============================================================================
// Literal text passed to the text-keyed helpers _(...) / _W(...) / TR(...) in
// C/C++ sources.
NLSI18N_IMPEXP std::set<std::string>
collectTextKeyedLiterals(const std::wstring& sourceRoot);
//=============================================================================
// Message templates of error/warning calls in C/C++ and .m sources,
// transparently unwrapping _(...)/_W(...)/sprintf(...) wrappers.
NLSI18N_IMPEXP std::set<std::string>
collectErrorWarningMessages(const std::wstring& sourceRoot);
//=============================================================================
} // namespace Nelson
//=============================================================================
