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
#include <filesystem>
#include <string>
#include "nlsFile_archiver_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
// UTF-8 text of a wide string (wchar_t is UTF-16 on Windows, UTF-32 elsewhere).
NLSFILE_ARCHIVER_IMPEXP std::string
wideToUtf8(const std::wstring& text);
NLSFILE_ARCHIVER_IMPEXP void
appendUtf8(std::string& out, char32_t codePoint);
// Path of a Nelson file name: UTF-8 based, so it does not depend on the C locale.
NLSFILE_ARCHIVER_IMPEXP std::filesystem::path
toNativePath(const std::wstring& filename);
//=============================================================================
} // namespace Nelson
//=============================================================================
