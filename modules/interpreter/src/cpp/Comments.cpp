//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <boost/algorithm/string.hpp>
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
    std::string str = boost::algorithm::trim_left_copy(line);
    return boost::algorithm::starts_with(str, "%");
}
//=============================================================================
bool
isCommentedLine(const std::wstring& line)
{
    std::wstring str = boost::algorithm::trim_left_copy(line);
    return boost::algorithm::starts_with(str, L"%");
}
//=============================================================================
} // namespace Nelson
//=============================================================================
