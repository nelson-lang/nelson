//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include "ArrayOf.hpp"
#include "Endian.hpp"
#include "File.hpp"
#include "nlsStream_manager_exports.h"
#include <string>
//=============================================================================
namespace Nelson {
enum FWRITE_ERROR_TYPE
{
    FWRITE_NO_ERROR,
    FWRITE_DATA_TYPE_NOT_SUPPORTED,
    FWRITE_FILE_DESTINATION_NOT_SUPPORTED,
    FWRITE_ALLOCATION_MEMORY,
    FWRITE_INVALID_FILE,
    FWRITE_ENDIAN_CONVERSION_NOT_SUPPORTED,
    FWRITE_ERROR_ENCODING
};
//=============================================================================
NLSSTREAM_MANAGER_IMPEXP bool
writeFile(const std::wstring& filename, const wstringVector& lines, const std::wstring& weol,
    const std::string& eol, const std::string& encoding, std::wstring& errorMessage);
//=============================================================================
NLSSTREAM_MANAGER_IMPEXP FWRITE_ERROR_TYPE
FileWrite(File* fp, ArrayOf src, NelsonType destClass, size_t skip, bool bIsLittleEndian,
    int& sizeWritten);
}; // namespace Nelson
//=============================================================================
