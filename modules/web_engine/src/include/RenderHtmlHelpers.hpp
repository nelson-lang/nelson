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
#include <filesystem>
#include <string>
//=============================================================================
namespace Nelson::WebviewRender {
//=============================================================================
std::string
fileUrlFromPath(const std::filesystem::path& path);
void
replaceMathJaxContainersWithImages(std::string& root);
bool
extractRenderedRoot(const std::string& dom, std::string& root);
//=============================================================================
} // namespace Nelson::WebviewRender
//=============================================================================
