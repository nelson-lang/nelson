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
#include <string>
#include <vector>
#include <sstream>
#include "nlsError_manager_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
NLSERROR_MANAGER_IMPEXP void
Error(const std::wstring& msg, const std::wstring& id = L"", bool asCaller = false) noexcept(false);
//=============================================================================
NLSERROR_MANAGER_IMPEXP void
Error(const std::string& msg, const std::string& id = "", bool asCaller = false) noexcept(false);
//=============================================================================
namespace detail {
    //=============================================================================
    // Direct string formatting without fmt templates
    NLSERROR_MANAGER_IMPEXP std::wstring
    formatToWideStringImpl(const std::wstring& format, const std::vector<std::wstring>& args);
    //=============================================================================
    NLSERROR_MANAGER_IMPEXP std::wstring
    formatToWideStringImplMessageID(const std::wstring& ID, const std::vector<std::wstring>& args);
    //=============================================================================
    // Helper: convert any type to wstring - must be defined inline in header
    template <typename T>
    inline std::wstring
    toWideString(const T& value)
    {
        std::wostringstream oss;
        oss << value;
        return oss.str();
    }
    //=============================================================================
    // Specialization for std::wstring
    template <>
    inline std::wstring
    toWideString<std::wstring>(const std::wstring& value)
    {
        return value;
    }
    //=============================================================================
} // namespace detail
//=============================================================================
// Variadic template - converts all args to wstring
template <typename... Args>
inline void
raiseError(const std::wstring& messageID, const std::wstring& messageFormat, const Args&... args)
{
    std::vector<std::wstring> argVec;
    // Fold expression: convert each argument using toWideString
    (..., argVec.push_back(detail::toWideString(args)));

    std::wstring formatted = detail::formatToWideStringImpl(messageFormat, argVec);
    Error(formatted, messageID, false);
}
//=============================================================================
// Variadic template - converts all args to wstring
template <typename... Args>
inline void
raiseError2(const std::wstring& messageID, const Args&... args)
{
    std::vector<std::wstring> argVec;
    // Fold expression: convert each argument using toWideString
    (..., argVec.push_back(detail::toWideString(args)));

    std::wstring formatted = detail::formatToWideStringImplMessageID(messageID, argVec);
    Error(formatted, messageID, false);
}
//=============================================================================
template <typename... Args>
inline void
raiseErrorAsCaller(bool asCaller, const std::wstring& messageID, const Args&... args)
{
    std::vector<std::wstring> argVec;
    // Fold expression: convert each argument using toWideString
    (..., argVec.push_back(detail::toWideString(args)));

    std::wstring formatted = detail::formatToWideStringImplMessageID(messageID, argVec);
    Error(formatted, messageID, asCaller);
}
//=============================================================================
// Function that returns the formatted error message as std::wstring
template <typename... Args>
inline std::wstring
formatErrorMessage(const std::wstring& messageID, const Args&... args)
{
    std::vector<std::wstring> argVec;
    // Fold expression: convert each argument using toWideString
    (..., argVec.push_back(detail::toWideString(args)));

    return detail::formatToWideStringImplMessageID(messageID, argVec);
}
//=============================================================================

} // namespace Nelson
//=============================================================================
