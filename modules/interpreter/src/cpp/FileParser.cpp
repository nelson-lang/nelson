//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "FileParser.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static std::wstring wfilename;
//=============================================================================
void
setParserFilename(const std::string& filename)
{
    wfilename = utf8_to_wstring(filename);
}
//=============================================================================
void
setParserFilename(const std::wstring& filename)
{
    wfilename = filename;
}
//=============================================================================
std::string
getParserFilenameU()
{
    return wstring_to_utf8(wfilename);
}
//=============================================================================
std::wstring
getParserFilenameW()
{
    return wfilename;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
