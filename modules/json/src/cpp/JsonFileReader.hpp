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
#include <simdjson.h>
//=============================================================================
namespace Nelson {
//=============================================================================
// Loads a whole file into a simdjson padded buffer.
// Returns false and sets errorMessage when the file cannot be read.
bool
readJsonFile(
    const std::wstring& filename, simdjson::padded_string& content, std::string& errorMessage);
//=============================================================================
} // namespace Nelson
//=============================================================================
