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
#include "XmlTarget.hpp"
#include <string>
//=============================================================================
namespace Nelson {
bool
XmlDocResolveLink(const std::wstring& directorysource, const std::wstring& linkname,
    const std::wstring& currentModuleName, DOCUMENT_OUTPUT outputTarget,
    const std::wstring& destinationDir, const std::wstring& language, std::wstring& resolvedlink);
}
//=============================================================================
