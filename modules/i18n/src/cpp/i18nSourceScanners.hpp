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
#include <map>
#include <set>
#include <string>
#include <vector>
//=============================================================================
// Internal source scanners shared by the i18n extraction and catalog
// maintenance passes. They operate on raw file content (UTF-8 bytes) and
// return decoded (real character) strings, so catalog keys are identical
// whatever the scanning pass.
//=============================================================================
namespace Nelson {
//=============================================================================
// C/C++: literal text passed to the text-keyed helpers _(...) / _W(...) / TR(...).
void
scanSourceForTextKeyed(const std::string& text, std::set<std::string>& texts);
//=============================================================================
// C/C++: identifiers used with a run-time (non-literal) message in
// Error(id, X) / ErrorAsCaller(id, X) / Warning(id, X).
void
scanSourceForRuntimeIds(const std::string& text, std::set<std::string>& ids);
//=============================================================================
// C/C++ or .m: message templates of error/warning calls, transparently
// unwrapping _(...)/_W(...)/sprintf(...) wrappers.
void
scanSourceForErrorMessages(const std::string& text, std::set<std::string>& msgs, bool mFile);
//=============================================================================
// C/C++: identifier/template literal pairs of Error / ErrorAsCaller / Warning /
// resolveMessageTemplate calls (argument 1 is the identifier, argument 2 the
// template; both must be plain string literals). Each identifier accumulates
// its distinct templates so overloaded identifiers can be reported and dropped.
void
scanSourceForIdTemplates(
    const std::string& text, std::map<std::string, std::vector<std::string>>& idTemplates);
//=============================================================================
// .m: literal messages of _('...') / _("...") calls. Language-aware scanning:
// string literals, comments, block comments, transpose operator and line
// continuations are understood, so only real messages are extracted.
void
scanMFileForUiStrings(const std::string& text, std::set<std::string>& texts);
//=============================================================================
// .m: identifier/template literal pairs of error('Nelson:...', 'tmpl') and
// warning('Nelson:...', 'tmpl') calls (bare literal template only), plus
// nmm_error('reason', 'tmpl', ...) keyed as Nelson:nmm:<reason>.
void
scanMFileForIdTemplates(
    const std::string& text, std::map<std::string, std::vector<std::string>>& idTemplates);
//=============================================================================
} // namespace Nelson
//=============================================================================
