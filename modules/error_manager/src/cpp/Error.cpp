//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <cwctype>
#include <algorithm>
#include "Error.hpp"
#include "Evaluator.hpp"
#include "DebugStack.hpp"
#include "characters_encoding.hpp"
#include "NelsonConfiguration.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
void
Error(const std::wstring& msg, const std::wstring& id, bool asCaller)
{
    if (!msg.empty()) {
        stackTrace trace;
        Evaluator* eval = (Evaluator*)NelsonConfiguration::getInstance()->getMainEvaluator();
        if (!eval) {
            return;
        }
        DebugStack(eval->callstack, asCaller ? 1 : 0, trace);
        Exception exception(msg, trace, id);
        throw exception;
    }
}
//=============================================================================
void
Error(const std::string& msg, const std::string& id, bool asCaller)
{
    Error(utf8_to_wstring(msg), utf8_to_wstring(id), asCaller);
}
//=============================================================================
namespace detail {
    //=============================================================================
    std::wstring
    formatToWideStringImpl(const std::wstring& format, const std::vector<std::wstring>& args)
    {
        try {
            std::wstring result = format;
            size_t argIndex = 0;
            size_t pos = 0;

            while ((pos = result.find(L"{", pos)) != std::wstring::npos) {
                size_t endPos = result.find(L"}", pos);
                if (endPos == std::wstring::npos) {
                    break;
                }

                std::wstring placeholder = result.substr(pos, endPos - pos + 1);
                std::wstring indexStr = result.substr(pos + 1, endPos - pos - 1);

                // Determine which argument to use
                size_t useArgIndex = argIndex;
                if (!indexStr.empty()) {
                    // Check if indexStr contains only digits (explicit index like {0}, {1})
                    bool isNumericOnly = std::all_of(indexStr.begin(), indexStr.end(),
                        [](wchar_t c) { return std::iswdigit(c); });

                    if (isNumericOnly) {
                        // Explicit index like {0}, {1}
                        try {
                            useArgIndex = std::stoul(indexStr);
                        } catch (...) {
                            pos = endPos + 1;
                            continue;
                        }
                    } else {
                        // Format spec without explicit index like {:<10}, {:.4f}
                        // Auto-increment
                        useArgIndex = argIndex++;
                    }
                } else {
                    // Auto-increment index for {}
                    useArgIndex = argIndex++;
                }

                // Replace if index is valid
                if (useArgIndex < args.size()) {
                    result.replace(pos, placeholder.length(), args[useArgIndex]);
                    pos += args[useArgIndex].length();
                } else {
                    pos = endPos + 1;
                }
            }

            return result;
        } catch (const std::exception&) {
            return format;
        }
    }
    //=============================================================================
} // namespace detail
//=============================================================================
} // namespace Nelson
//=============================================================================
