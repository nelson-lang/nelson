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
#include "nlsParquet_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
struct ParquetWriteOptions
{
    std::string compression;
    std::string encoding;
    std::string version;
    std::vector<int64> rowGroupHeights;
};
//=============================================================================
NLSPARQUET_IMPEXP void
ParquetWrite(const std::wstring& filename, const ArrayOf& tableValue,
    const ParquetWriteOptions& options, std::wstring& errorMessage);
//=============================================================================
} // namespace Nelson
//=============================================================================
