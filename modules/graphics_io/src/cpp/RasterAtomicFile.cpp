//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "RasterAtomicFile.hpp"
#include <chrono>
#include <functional>
#include <thread>
#ifdef _WIN32
#include <windows.h>
#endif
//=============================================================================
namespace Nelson {
//=============================================================================
std::filesystem::path
rasterTemporaryPath(const std::filesystem::path& target)
{
    const auto now = std::chrono::steady_clock::now().time_since_epoch().count();
    const auto thread = std::hash<std::thread::id> {}(std::this_thread::get_id());
    return std::filesystem::path(
        target.wstring() + L".nelson-tmp-" + std::to_wstring(now) + L"-" + std::to_wstring(thread));
}
//=============================================================================
bool
replaceFileAtomically(
    const std::filesystem::path& temporary, const std::filesystem::path& target, std::string& error)
{
#ifdef _WIN32
    if (MoveFileExW(temporary.c_str(), target.c_str(),
            MOVEFILE_REPLACE_EXISTING | MOVEFILE_WRITE_THROUGH)) {
        return true;
    }
    const unsigned long code = GetLastError();
    std::error_code cleanup;
    std::filesystem::remove(temporary, cleanup);
    error = "cannot atomically replace output file (Windows error " + std::to_string(code) + ")";
    return false;
#else
    std::error_code ec;
    std::filesystem::rename(temporary, target, ec);
    if (!ec) {
        return true;
    }
    std::error_code cleanup;
    std::filesystem::remove(temporary, cleanup);
    error = "cannot atomically replace output file: " + ec.message();
    return false;
#endif
}
//=============================================================================
} // namespace Nelson
//=============================================================================
