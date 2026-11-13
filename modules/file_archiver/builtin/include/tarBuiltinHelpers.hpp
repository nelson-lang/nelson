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
#include "ZipArchive.hpp"
//=============================================================================
// Error translation shared by the nelson.io.tar builtins.
namespace Nelson::FileArchiverGateway {
//=============================================================================
// Translates an archive error of a tar archive; source names the archive for open errors.
[[noreturn]] void
raiseTarError(const ZipArchiveError& error, const std::wstring& source = L"");
//=============================================================================
} // namespace Nelson::FileArchiverGateway
//=============================================================================
