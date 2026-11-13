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
#ifdef _WIN32
#ifndef NOMINMAX
// archive.h includes windows.h: keep std::min and std::max usable.
#define NOMINMAX
#endif
#endif
#include <archive.h>
#include <archive_entry.h>
#include <string>
#include "ZipArchive.hpp"
//=============================================================================
// Helpers shared by the code calling libarchive.
namespace Nelson {
//=============================================================================
// Last libarchive message, or fallback when there is none.
std::string
libarchiveErrorText(struct archive* handle, const char* fallback);
//=============================================================================
// Error code of a failed read: wrong or missing password, bad checksum, or other.
ZipErrorCode
libarchiveReadErrorCode(struct archive* handle);
//=============================================================================
} // namespace Nelson
//=============================================================================
