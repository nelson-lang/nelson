//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <vector>
#include <algorithm>
#include <string>
#include <unordered_map>
#include <cppenv/cppenv.hpp>
#include "LoadEnvironment.hpp"
#include "characters_encoding.hpp"
#include "i18n.hpp"
#include "SetVariableEnvironment.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
std::vector<std::pair<std::wstring, std::wstring>>
LoadEnvironment(const std::wstring& filename, bool applyToEnv, std::wstring& errorMessage)
{
    std::vector<std::pair<std::wstring, std::wstring>> result;
    try {
        cppenv::EnvManager envManager;
        bool loaded = envManager.load_from_file(filename);
        if (!loaded) {
            errorMessage = _W("Failed to load environment file: ") + filename;
            return {};
        }

        std::vector<std::string> keys = envManager.names();
        result.reserve(keys.size());

        auto trim_and_unquote = [](std::wstring s) -> std::wstring {
            // trim whitespace
            const std::wstring ws = L" \t\r\n";
            auto start = s.find_first_not_of(ws);
            if (start == std::wstring::npos)
                return L"";
            auto end = s.find_last_not_of(ws);
            s = s.substr(start, end - start + 1);
            // remove matching surrounding single or double quotes
            if (s.size() >= 2) {
                if ((s.front() == L'\'' && s.back() == L'\'')
                    || (s.front() == L'"' && s.back() == L'"')) {
                    s = s.substr(1, s.size() - 2);
                }
            }
            return s;
        };

        for (const auto& key : keys) {
            auto valueOpt = envManager.get_value_as<std::string>(key);
            if (!valueOpt.has_value())
                continue;
            std::wstring wkey = utf8_to_wstring(key);
            std::wstring val = utf8_to_wstring(valueOpt.value());
            val = trim_and_unquote(val);
            result.emplace_back(std::move(wkey), std::move(val));
        }

        if (applyToEnv) {
            for (const auto& kv : result) {
                SetVariableEnvironmentW(kv.first, kv.second);
            }
        }
    } catch (const std::exception& ex) {
        errorMessage
            = _W("Exception while loading environment: ") + utf8_to_wstring(std::string(ex.what()));
        return {};
    } catch (...) {
        errorMessage = _W("Unknown error while loading environment file: ") + filename;
        return {};
    }

    return result;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
