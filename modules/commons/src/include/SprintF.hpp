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
template <typename T>
auto
to_utf8_arg(T&& v)
{
    return utf8_arg<T>::convert(std::forward<T>(v));
}
//=============================================================================
// fmt::sprintf does not accept wstring: format and arguments go through UTF-8.
// fmt::format is faster when the format string is known at compile time.
template <typename... Args>
std::wstring
sprintf_w_utf8(const std::wstring& wfmt, Args&&... args)
{
    std::string fmt_utf8 = wstring_to_utf8(wfmt);
    auto utf8_args = std::make_tuple(to_utf8_arg(std::forward<Args>(args))...);
    std::string result_utf8 = std::apply(
        [&](auto&&... a) { return fmt::sprintf(fmt_utf8, std::forward<decltype(a)>(a)...); },
        utf8_args);
    return utf8_to_wstring(result_utf8);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
