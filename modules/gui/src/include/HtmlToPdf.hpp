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
#include "nlsGui_exports.h"
#include <string>
//=============================================================================
namespace Nelson {
extern "C"
{
    NLSGUI_IMPEXP bool
    HtmlFileToPdfFile(const std::wstring& htmlsrcfilename, const std::wstring& pdfdestfilename);
    NLSGUI_IMPEXP bool
    HtmlStreamToPdfFile(const std::wstring& htmlstream, const std::wstring& pdfdestfilename);
}

} // namespace Nelson
//=============================================================================
