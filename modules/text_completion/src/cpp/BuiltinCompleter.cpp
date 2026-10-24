//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <algorithm>
#include <cstdint>
#include <limits>
#include <mutex>
#include "BuiltinCompleter.hpp"
#include "Evaluator.hpp"
#include "CompleterHelper.hpp"
#include "What.hpp"
#include "StringHelpers.hpp"
#include "BuiltInFunctionDefManager.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
namespace {
    //=============================================================================
    std::mutex builtinCacheMutex;
    uint64_t builtinCacheGeneration = std::numeric_limits<uint64_t>::max();
    wstringVector builtinCacheNames;
    // Uppercased shadow of builtinCacheNames so the case-insensitive match
    // costs one conversion per request instead of two per candidate.
    wstringVector builtinCacheUpperNames;
    //=============================================================================
    // Sorted builtin names without internal '__' entries, rebuilt only when the
    // builtin table generation moves (module load/unload).
    void
    refreshCachedBuiltinNames(Evaluator* eval)
    {
        uint64_t generation = BuiltInFunctionDefManager::getInstance()->generation();
        if (generation != builtinCacheGeneration) {
            wstringVector names = WhatListOfBuiltin(eval, true, true);
            names.erase(std::remove_if(names.begin(), names.end(),
                            [](const std::wstring& name) {
                                return StringHelpers::starts_with(name, L"__");
                            }),
                names.end());
            builtinCacheNames = std::move(names);
            builtinCacheUpperNames.clear();
            builtinCacheUpperNames.reserve(builtinCacheNames.size());
            for (const auto& name : builtinCacheNames) {
                builtinCacheUpperNames.emplace_back(StringHelpers::to_upper_copy(name));
            }
            builtinCacheGeneration = generation;
        }
    }
    //=============================================================================
} // namespace
//=============================================================================
wstringVector
BuiltinCompleter(const std::wstring& prefix)
{
    wstringVector res;
    auto* eval = getCompletionEvaluator();
    if (eval) {
        std::scoped_lock<std::mutex> lock(builtinCacheMutex);
        refreshCachedBuiltinNames(eval);
        const std::wstring upperPrefix = StringHelpers::to_upper_copy(prefix);
        for (size_t k = 0; k < builtinCacheNames.size(); ++k) {
            if (builtinCacheUpperNames[k].find(upperPrefix) != std::wstring::npos) {
                res.push_back(builtinCacheNames[k]);
            }
        }
    }
    return res;
}
//=============================================================================
} // namespace Nelson;
//=============================================================================
