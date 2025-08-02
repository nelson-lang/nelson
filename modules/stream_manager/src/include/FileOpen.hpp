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
#include "ArrayOf.hpp"
#include "FilesManager.hpp"
#include "nlsStream_manager_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
enum FOPEN_ERROR_TYPE
{
    FOPEN_NO_ERROR,
    FOPEN_INVALID_NAME,
    FOPEN_INVALID_MODE,
    FOPEN_INVALID_MACHINE_FORMAT,
    FOPEN_INVALID_ENCODING,
    FOPEN_IMPOSSIBLE_TO_ADD_FILE,
    FOPEN_CANNOT_OPEN
};
//=============================================================================
NLSSTREAM_MANAGER_IMPEXP FOPEN_ERROR_TYPE
FileOpen(FilesManager* fm, const std::wstring& filename, const std::wstring& filemode,
    const std::wstring& machineFormat, const std::wstring& encoding, int& fileposition);
//=============================================================================
}; // namespace Nelson
//=============================================================================
