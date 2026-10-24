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
#include "nlsError_manager_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
// Low-level throw: an exact message with an optional identifier. Use it when the
// message is built at run time (dynamic text) rather than resolved from the
// identifier catalog. asCaller = true reports the error at the caller's frame.
NLSERROR_MANAGER_IMPEXP void
RaiseError(const std::wstring& msg, const std::wstring& id = L"", bool asCaller = false) noexcept(
    false);
//=============================================================================
NLSERROR_MANAGER_IMPEXP void
RaiseError(const std::string& msg, const std::string& id = "", bool asCaller = false) noexcept(
    false);
//=============================================================================
// Message-only error (no identifier).
inline void
Error(const std::wstring& msg) noexcept(false)
{
    RaiseError(msg);
}
//=============================================================================
inline void
Error(const std::string& msg) noexcept(false)
{
    RaiseError(msg);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
