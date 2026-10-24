//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
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
#include "SimEngineTypes.hpp"
//=============================================================================
namespace Nelson {
namespace NFlow {
    //=============================================================================
    // Resolves an asset path of a block: a packaged example asset
    // (nflow-example:/<namespace>/...) maps onto the example roots of the context,
    // a relative path is taken from the diagram directory, anything else is kept.
    inline std::filesystem::path
    resolveAssetPath(const SimCtx& ctx, const std::string& path)
    {
        static const std::string prefix = "nflow-example:/";
#ifdef __EMSCRIPTEN__
        // the browser build hands packaged example assets to its JavaScript provider as is
        if (path.compare(0, prefix.size(), prefix) == 0) {
            return std::filesystem::u8path(path);
        }
#endif
        if (path.compare(0, prefix.size(), prefix) == 0 && ctx.exampleRoots) {
            const std::string rest = path.substr(prefix.size());
            bool safe = !rest.empty();
            size_t start = 0;
            while (safe && start <= rest.size()) {
                size_t end = rest.find('/', start);
                if (end == std::string::npos) {
                    end = rest.size();
                }
                const std::string segment = rest.substr(start, end - start);
                safe = !segment.empty() && segment != "." && segment != "..";
                start = end + 1;
            }
            if (safe) {
                const std::string ns = rest.substr(0, rest.find('/'));
                auto it = ctx.exampleRoots->find(ns);
                if (it != ctx.exampleRoots->end() && ns.size() < rest.size()) {
                    return std::filesystem::u8path(it->second)
                        / std::filesystem::u8path(rest.substr(ns.size() + 1));
                }
                it = ctx.exampleRoots->find(std::string());
                if (it != ctx.exampleRoots->end()) {
                    return std::filesystem::u8path(it->second) / std::filesystem::u8path(rest);
                }
            }
        }
        std::filesystem::path resolved(std::filesystem::u8path(path));
        if (resolved.is_relative() && ctx.baseDir && !ctx.baseDir->empty()) {
            resolved = std::filesystem::u8path(*ctx.baseDir) / resolved;
        }
        return resolved;
    }
    //=============================================================================
} // namespace NFlow
} // namespace Nelson
//=============================================================================
