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
//=============================================================================
namespace Nelson {
//=============================================================================
typedef enum
{
    HTML_WEB,
    MARKDOWN,
    TYPST,
    UNDEFINED
} DOCUMENT_OUTPUT;
//=============================================================================
inline std::wstring
documentOutputExtension(DOCUMENT_OUTPUT documentOutput)
{
    switch (documentOutput) {
    case DOCUMENT_OUTPUT::MARKDOWN:
        return L".md";
    case DOCUMENT_OUTPUT::TYPST:
        return L".typ";
    case DOCUMENT_OUTPUT::HTML_WEB:
    default:
        return L".html";
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
