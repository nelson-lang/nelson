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
#include <cstdint>
#include <string>
#include "ArrayOf.hpp"
#include "FileBackend.hpp"
#include "ExtractionGuard.hpp"
//=============================================================================
// Argument checks shared by the nelson.io.zip and nelson.io.zstd builtins.
namespace Nelson::FileArchiverGateway {
//=============================================================================
// MaxSize value: Inf (no limit) or a nonnegative integer number of bytes.
uint64_t
sizeLimit(const ArrayOf& value);
//=============================================================================
// MaxExtractedSize (Inf or bytes) and MaxCompressionRatio (Inf or a positive number).
ExtractionLimits
extractionLimits(const ArrayOf& maxExtractedSize, const ArrayOf& maxCompressionRatio);
//=============================================================================
// 'r', 'w' or, when allowed, 'a' (each with an optional 'b'); returns that letter.
wchar_t
fileMode(const ArrayOf& mode, bool allowAppend);
//=============================================================================
// Encoding name accepted by fopen.
std::wstring
checkedEncoding(const ArrayOf& encoding);
//=============================================================================
// Registers backend (ownership taken) in the file manager; returns its identifier.
ArrayOf
openFileIdentifier(
    FileBackend* backend, const std::wstring& name, wchar_t mode, const std::wstring& encoding);
//=============================================================================
} // namespace Nelson::FileArchiverGateway
//=============================================================================
