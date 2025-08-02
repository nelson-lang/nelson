//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#define _SCL_SECURE_NO_WARNINGS
//=============================================================================
#include "XmlDocChapterNamer.hpp"
#include "UuidHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
std::wstring
XmlDocChapterNamer(const std::wstring& dstDirectory)
{
    std::wstring guid;
    UuidHelpers::generateUuid(guid);
    return dstDirectory + L"chapter-" + guid + L".html";
}
//=============================================================================
}
//=============================================================================
