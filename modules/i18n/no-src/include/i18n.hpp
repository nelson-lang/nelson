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
NLSI18N_IMPEXP std::wstring
gettextw(const std::wstring& S);
NLSI18N_IMPEXP std::wstring
gettextw(const std::string& S);
NLSI18N_IMPEXP std::string
gettext(const std::string& S);
//=============================================================================
// This ICU-less build ships no error/warning catalog, so these return an empty
// string and callers fall back to their own message text. The symbols must match
// the full i18n library: the whole core links against them.
NLSI18N_IMPEXP std::wstring
getMessageTemplateById(const std::wstring& id);
NLSI18N_IMPEXP std::string
getMessageTemplateById(const std::string& id);
NLSI18N_IMPEXP std::wstring
getEnglishMessageTemplateById(const std::wstring& id);
//=============================================================================
} // namespace Nelson
//=============================================================================
#define _W(T) Nelson::gettextw(T)
#define _(T) Nelson::gettext(T)
//=============================================================================
