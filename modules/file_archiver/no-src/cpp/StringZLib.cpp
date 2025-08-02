//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <iostream>
#include <iomanip>
#include <sstream>
#include <zlib.h>
#include <cstring>
#include "StringZLib.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
#define BUFFER_SIZE 8192
//=============================================================================
std::string
compressString(const std::string& str, bool& failed)
{
    failed = false;
    return str;
}
//=============================================================================
std::string
decompressString(const std::string& str, bool& failed)
{
    failed = false;
    return str;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
