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
#include <vector>
#include "ArrayOf.hpp"
#include "Types.hpp"
#include "ZipArchive.hpp"
//=============================================================================
// Argument parsing and error translation shared by the nelson.io.zip builtins.
namespace Nelson::FileArchiverGateway {
//=============================================================================
// Translates a ZipArchiveError into a Nelson error with a stable identifier.
[[noreturn]] void
raiseZipError(const ZipArchiveError& error);
//=============================================================================
uint64_t
archiveId(const ArrayOf& value);
//=============================================================================
// uint8 data as bytes; char or string as UTF-8.
std::vector<uint8_t>
byteVector(const ArrayOf& value);
//=============================================================================
// Compression name and level, checked against the build and the method range.
ZipWriteOptions
writeOptions(const ArrayOf& compression, const ArrayOf& level);
//=============================================================================
ArrayOf
stringColumn(const wstringVector& values);
//=============================================================================
} // namespace Nelson::FileArchiverGateway
//=============================================================================
