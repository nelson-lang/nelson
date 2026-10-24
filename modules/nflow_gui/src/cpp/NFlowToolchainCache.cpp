//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "NFlowToolchainCache.hpp"
#include "characters_encoding.hpp"
#include <nlohmann/json.hpp>
#include <filesystem>
#include <fstream>
//=============================================================================
namespace Nelson::NFlow {
//=============================================================================
static std::filesystem::path
toolchainCachePath(const std::string& preferencesDirectory);
//=============================================================================
ToolchainCache
loadToolchainCache(const std::string& preferencesDirectory)
{
    ToolchainCache cache;
    try {
        std::ifstream stream(toolchainCachePath(preferencesDirectory), std::ios::binary);
        if (!stream) {
            return cache;
        }
        const auto data = nlohmann::json::parse(stream);
        cache.haveC = data.value("haveCToolChain", false);
        cache.haveRust = data.value("haveRustToolChain", false);
    } catch (...) {
        // Missing/corrupt cache: fall back to "not available".
    }
    return cache;
}
//=============================================================================
void
saveToolchainCache(const std::string& preferencesDirectory, bool haveC, bool haveRust)
{
    try {
        const nlohmann::json data { { "haveCToolChain", haveC },
            { "haveRustToolChain", haveRust } };
        std::ofstream stream(
            toolchainCachePath(preferencesDirectory), std::ios::binary | std::ios::trunc);
        if (stream) {
            stream << data.dump(2);
        }
    } catch (...) {
    }
}
//=============================================================================
std::string
buildToolchainStoragePushJs(bool haveC, bool haveRust)
{
    const std::string cv = haveC ? "1" : "0";
    const std::string rv = haveRust ? "1" : "0";
    return std::string("try{") + "sessionStorage.setItem('nf-have-c-toolchain','" + cv + "');"
        + "localStorage.setItem('nf-have-c-toolchain','" + cv + "');"
        + "sessionStorage.setItem('nf-have-rust-toolchain','" + rv + "');"
        + "localStorage.setItem('nf-have-rust-toolchain','" + rv + "');"
        + "window.dispatchEvent(new StorageEvent('storage',{key:'nf-have-c-toolchain',newValue:'"
        + cv + "'}));"
        + "window.dispatchEvent(new StorageEvent('storage',{key:'nf-have-rust-toolchain',newValue:'"
        + rv + "'}));" + "}catch(e){}";
}
//=============================================================================
// static helpers (definitions at the bottom; forward-declared above)
//=============================================================================
static std::filesystem::path
toolchainCachePath(const std::string& preferencesDirectory)
{
#ifdef _MSC_VER
    std::filesystem::path path(utf8_to_wstring(preferencesDirectory));
#else
    std::filesystem::path path(preferencesDirectory);
#endif
    path /= "nflow-toolchain-cache.json";
    return path;
}
//=============================================================================
} // namespace Nelson::NFlow
//=============================================================================
