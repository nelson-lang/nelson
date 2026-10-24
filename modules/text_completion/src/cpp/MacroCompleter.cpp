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
#include <unordered_map>
#include "MacroCompleter.hpp"
#include "ClassdefParser.hpp"
#include "NelsonException.hpp"
#include "FunctionsInMemory.hpp"
#include "PathFunctionIndexerManager.hpp"
#include "StringHelpers.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
namespace {
    //=============================================================================
    bool
    isHiddenClassdefConstructor(const std::wstring& macro)
    {
        try {
            std::string name = wstring_to_utf8(macro);
            std::wstring filename;
            if (!PathFunctionIndexerManager::getInstance()->find(name, filename)) {
                return false;
            }
            if (!classdefIsClassdefFile(filename) || !classdefLoadClass(name)) {
                return false;
            }
            stringVector attributes = classdefClassAttributes(name);
            return std::find(attributes.begin(), attributes.end(), "Hidden") != attributes.end();
        } catch (const NelsonException&) {
            return false;
        }
    }
    //=============================================================================
    std::mutex macroCacheMutex;
    uint64_t macroCacheGeneration = std::numeric_limits<uint64_t>::max();
    wstringVector macroCacheNames;
    // Uppercased shadow of macroCacheNames so the case-insensitive match costs
    // one conversion per request instead of two per candidate.
    wstringVector macroCacheUpperNames;
    // The hidden-classdef verdict opens and parses the candidate file, so it
    // is only computed for matched candidates and memoized per generation.
    std::unordered_map<std::wstring, bool> macroHiddenVerdicts;
    //=============================================================================
    // Macro names without '@' overloads, rebuilt only when the path function
    // tables generation moves (addpath/rmpath/cd/rehash).
    void
    refreshCachedMacroNames()
    {
        uint64_t generation = FunctionsInMemory::getInstance()->generation();
        if (generation != macroCacheGeneration) {
            wstringVector macros = PathFunctionIndexerManager::getInstance()->getMacrosList(false);
            macros.erase(std::remove_if(macros.begin(), macros.end(),
                             [](const std::wstring& macro) {
                                 return macro.empty() || macro.front() == L'@';
                             }),
                macros.end());
            macroCacheNames = std::move(macros);
            macroCacheUpperNames.clear();
            macroCacheUpperNames.reserve(macroCacheNames.size());
            for (const auto& macro : macroCacheNames) {
                macroCacheUpperNames.emplace_back(StringHelpers::to_upper_copy(macro));
            }
            macroHiddenVerdicts.clear();
            macroCacheGeneration = generation;
        }
    }
    //=============================================================================
    bool
    isHiddenClassdefConstructorCached(const std::wstring& macro)
    {
        auto it = macroHiddenVerdicts.find(macro);
        if (it != macroHiddenVerdicts.end()) {
            return it->second;
        }
        bool hidden = isHiddenClassdefConstructor(macro);
        macroHiddenVerdicts.emplace(macro, hidden);
        return hidden;
    }
    //=============================================================================
} // namespace
//=============================================================================
wstringVector
MacroCompleter(const std::wstring& prefix)
{
    wstringVector res;
    std::scoped_lock<std::mutex> lock(macroCacheMutex);
    refreshCachedMacroNames();
    const std::wstring upperPrefix = StringHelpers::to_upper_copy(prefix);
    res.reserve(macroCacheNames.size());
    for (size_t k = 0; k < macroCacheNames.size(); ++k) {
        if (macroCacheUpperNames[k].find(upperPrefix) != std::wstring::npos
            && !isHiddenClassdefConstructorCached(macroCacheNames[k])) {
            res.emplace_back(macroCacheNames[k]);
        }
    }
    return res;
}
//=============================================================================
} // namespace Nelson;
//=============================================================================
