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
#define FMT_HEADER_ONLY
#include <fmt/printf.h>
#include <fmt/format.h>
//=============================================================================
#include <string>
#include <string_view>
#include <tuple>
#include <type_traits>
#include <utility>
//=============================================================================
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
template <typename T, typename Enable = void> struct utf8_arg
{
    static T&&
    convert(T&& v)
    {
        return std::forward<T>(v);
    }
};
//=============================================================================
// std::wstring
template <typename T>
struct utf8_arg<T, std::enable_if_t<std::is_same_v<std::decay_t<T>, std::wstring>>>
{
    static std::string
    convert(const std::wstring& w)
    {
        return wstring_to_utf8(w);
    }
};
//=============================================================================
// std::wstring_view (copy required)
template <typename T>
struct utf8_arg<T, std::enable_if_t<std::is_same_v<std::decay_t<T>, std::wstring_view>>>
{
    static std::string
    convert(std::wstring_view w)
    {
        return wstring_to_utf8(std::wstring(w));
    }
};
//=============================================================================
// const wchar_t*
template <typename T>
struct utf8_arg<T, std::enable_if_t<std::is_same_v<std::decay_t<T>, const wchar_t*>>>
{
    static std::string
    convert(const wchar_t* w)
    {
        return w ? wstring_to_utf8(w) : std::string {};
    }
};
//=============================================================================
// Helper
template <typename T>
auto
to_utf8_arg(T&& v)
{
    return utf8_arg<T>::convert(std::forward<T>(v));
}
//=============================================================================
// fmt sprintf no support wstring -> use fmt::format or this workaround
// for speed, use fmt::format
// Public API
template <typename... Args>
std::wstring
sprintf_w_utf8(const std::wstring& wfmt, Args&&... args)
{
    // 1) Convert format string (UTF-16 -> UTF-8, optimized)
    std::string fmt_utf8 = wstring_to_utf8(wfmt);
    // 2) Convert arguments (wstring / wstring_view safe)
    auto utf8_args = std::make_tuple(to_utf8_arg(std::forward<Args>(args))...);
    // 3) Format using fmt::sprintf
    std::string result_utf8 = std::apply(
        [&](auto&&... a) { return fmt::sprintf(fmt_utf8, std::forward<decltype(a)>(a)...); },
        utf8_args);
    // 4) Convert result back to UTF-16
    return utf8_to_wstring(result_utf8);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
