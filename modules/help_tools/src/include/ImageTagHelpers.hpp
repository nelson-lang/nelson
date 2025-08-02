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
#include "Types.hpp"
#include <string>
//=============================================================================
namespace Nelson {
bool
isValidImageTag(const std::wstring& tag);
bool
parseImageTag(const std::wstring& tag, const std::wstring& srcDirectory, std::wstring& oldPath,
    std::wstring& newPath);
bool
findImageTag(const std::wstring& text, wstringVector& imagesTag);
bool
copyImages(const wstringVector& srcImages, const wstringVector& dstImages);
bool
copyImage(const std::wstring& srcImage, const std::wstring& dstImage);
std::wstring
crcFile(const std::wstring& filename);
} // namespace Nelson
//=============================================================================
