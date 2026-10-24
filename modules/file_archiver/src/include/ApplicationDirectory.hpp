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
#include <stdexcept>
#ifndef _WIN32
#include <cerrno>
#include <sys/stat.h>
#endif
namespace Nelson::ApplicationArchive {
inline void
createPrivateDirectory(const std::filesystem::path& path)
{
#ifdef _WIN32
    if (!std::filesystem::create_directory(path)) {
        throw std::runtime_error("Application directory must not already exist.");
    }
#else
    if (::mkdir(path.c_str(), S_IRWXU) != 0) {
        throw std::filesystem::filesystem_error("Cannot create private application directory.",
            path, std::error_code(errno, std::generic_category()));
    }
#endif
}
//=============================================================================
} // namespace Nelson::ApplicationArchive
//=============================================================================
