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
#include "TarArchive.hpp"
#include "Types.hpp"
#include "ExtractionGuard.hpp"
#include "nlsFile_archiver_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
// Writes a tar archive (compressed after .tar.gz/.tgz or .tar.zst/.tzst) like zip does.
NLSFILE_ARCHIVER_IMPEXP void
Tar(const std::wstring& tarFilename, const wstringVector& names, const std::wstring& rootpath,
    wstringVector& entrynames);
// Extracts a tar archive file under rootpath; filenames lists the files and folders written.
NLSFILE_ARCHIVER_IMPEXP void
UnTar(const std::wstring& tarFilename, const std::wstring& rootpath, wstringVector& filenames,
    const ExtractionLimits& limits = ExtractionLimits());
// Extracts an open archive (links are skipped, unsafe names refused).
NLSFILE_ARCHIVER_IMPEXP void
ExtractTar(TarArchive& archive, const std::wstring& rootpath, wstringVector& filenames,
    const ExtractionLimits& limits = ExtractionLimits());
//=============================================================================
} // namespace Nelson
//=============================================================================
