//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "StringHelpers.hpp"
#include "Comments.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
std::string
getCommentSymbol()
{
    return "%";
}
//=============================================================================
bool
isCommentedLine(const std::string& line)
{
    std::string str = StringHelpers::trim_left_copy(line);
    return StringHelpers::starts_with(str, "%");
}
//=============================================================================
bool
isCommentedLine(const std::wstring& line)
{
    std::wstring str = StringHelpers::trim_left_copy(line);
    return StringHelpers::starts_with(str, L"%");
}
//=============================================================================
} // namespace Nelson
//=============================================================================
