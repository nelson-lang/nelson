//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <sstream>
#include <algorithm>
#include <functional>
#include <cwctype>
#include <fmt/format.h>
#include <fmt/xchar.h>
#include "StringHelpers.hpp"
#include "ParallelTransform.hpp"
//=============================================================================
namespace Nelson::StringHelpers {
//=============================================================================
template <class T>
bool
starts_with(const T& mainStr, const T& toMatch)
{
    return (mainStr.find(toMatch) == 0);
}
//=============================================================================
bool
starts_with(const std::wstring& mainStr, const std::wstring& toMatch)
{
    return starts_with<std::wstring>(mainStr, toMatch);
}
//=============================================================================
bool
starts_with(const std::string& mainStr, const std::string& toMatch)
{
    return starts_with<std::string>(mainStr, toMatch);
}
//=============================================================================
bool
istarts_with(const std::wstring& mainStr, const std::wstring& toMatch)
{
    std::wstring MAINSTR = StringHelpers::to_upper_copy(mainStr);
    std::wstring TOMATCH = StringHelpers::to_upper_copy(toMatch);
    return (MAINSTR.find(TOMATCH) == 0);
}
//=============================================================================
template <class T>
bool
ends_with(const T& mainStr, const T& toMatch)
{
    if (mainStr.size() < toMatch.size()) {
        return false;
    }
    return (mainStr.rfind(toMatch) == (mainStr.size() - toMatch.size()));
}
//=============================================================================
bool
iends_with(const std::wstring& mainStr, const std::wstring& toMatch)
{
    std::wstring MAINSTR = StringHelpers::to_upper_copy(mainStr);
    std::wstring TOMATCH = StringHelpers::to_upper_copy(toMatch);
    return ends_with<std::wstring>(MAINSTR, TOMATCH);
}
//=============================================================================
bool
iends_with(const std::string& mainStr, const std::string& toMatch)
{
    std::string MAINSTR = StringHelpers::to_upper_copy(mainStr);
    std::string TOMATCH = StringHelpers::to_upper_copy(toMatch);
    return ends_with<std::string>(MAINSTR, TOMATCH);
}
//=============================================================================
bool
ends_with(const std::wstring& mainStr, const std::wstring& toMatch)
{
    return ends_with<std::wstring>(mainStr, toMatch);
}
//=============================================================================
bool
ends_with(const std::string& mainStr, const std::string& toMatch)
{
    return ends_with<std::string>(mainStr, toMatch);
}
//=============================================================================
void
to_upper(std::wstring& str)
{
    parallelTransform(str.begin(), str.end(), str.begin(), ::towupper);
}
//=============================================================================
void
to_lower(std::wstring& str)
{
    parallelTransform(str.begin(), str.end(), str.begin(), ::towlower);
}
//=============================================================================
std::wstring
to_lower_copy(const std::wstring& str)
{
    std::wstring copy_str(str);
    parallelTransform(copy_str.begin(), copy_str.end(), copy_str.begin(), ::towlower);
    return copy_str;
}
//=============================================================================
std::string
to_upper_copy(const std::string& str)
{
    std::string copy_str(str);
    parallelTransform(copy_str.begin(), copy_str.end(), copy_str.begin(), ::toupper);
    return copy_str;
}
//=============================================================================
std::wstring
to_upper_copy(const std::wstring& str)
{
    std::wstring copy_str(str);
    parallelTransform(copy_str.begin(), copy_str.end(), copy_str.begin(), ::towupper);
    return copy_str;
}
//=============================================================================
std::string
to_lower_copy(const std::string& str)
{
    std::string copy_str(str);
    parallelTransform(copy_str.begin(), copy_str.end(), copy_str.begin(), ::tolower);
    return copy_str;
}
//=============================================================================
template <class T>
bool
replace_first(T& str, const T& from, const T& to, size_t npos)
{
    size_t start_pos = str.find(from);
    if (start_pos == npos) {
        return false;
    }
    str.replace(start_pos, from.length(), to);
    return true;
}
//=============================================================================
template <class T>
bool
replace_last(T& str, const T& from, const T& to, size_t npos)
{
    size_t start_pos = str.rfind(from);
    if (start_pos == npos) {
        return false;
    }
    str.replace(start_pos, from.length(), to);
    return true;
}
//=============================================================================
template <class T>
void
replace_all(T& str, const T& from, const T& to, size_t npos)
{
    if (from.empty()) {
        if (to.empty()) {
            return; // Avoid infinite loop
        }
        T result;
        result.reserve((str.length() + 1) * to.length() + str.length());
        for (const auto& c : str) {
            result.append(to);
            result += c;
        }
        result.append(to);
        str = result;
        return;
    }

    // ⚡ Bolt: This implementation was optimized to reduce redundant string searches.
    // Instead of iterating to count occurrences and then iterating again to replace,
    // this version finds all occurrences in a single pass, stores their positions,
    // and then constructs the new string. This is particularly efficient when the
    // replacement string is longer than the search string, as it avoids multiple
    // reallocations and searches.

    std::vector<size_t> positions;
    size_t pos = str.find(from, 0);
    while(pos != npos) {
        positions.push_back(pos);
        pos = str.find(from, pos + from.length());
    }

    if (positions.empty()) {
        return;
    }

    T result;
    const size_t originalLength = str.length();
    const size_t fromLength = from.length();
    const size_t toLength = to.length();
    const size_t occurrences = positions.size();

    size_t finalLength;
    if (toLength >= fromLength) {
        finalLength = originalLength + occurrences * (toLength - fromLength);
    } else {
        finalLength = originalLength - occurrences * (fromLength - toLength);
    }

    result.reserve(finalLength);
    size_t last_pos = 0;
    for (size_t current_pos : positions) {
        result.append(str, last_pos, current_pos - last_pos);
        result.append(to);
        last_pos = current_pos + from.length();
    }
    result.append(str, last_pos, str.length() - last_pos);

    str = result;
}
//=============================================================================
bool
replace_first(std::wstring& str, const std::wstring& from, const std::wstring& to)
{
    return replace_first<std::wstring>(str, from, to, std::wstring::npos);
}
//=============================================================================
bool
replace_last(std::wstring& str, const std::wstring& from, const std::wstring& to)
{
    return replace_last<std::wstring>(str, from, to, std::wstring::npos);
}
//=============================================================================
void
replace_all(std::wstring& str, const std::wstring& from, const std::wstring& to)
{
    replace_all<std::wstring>(str, from, to, std::wstring::npos);
}
//=============================================================================
std::wstring
replace_all_copy(const std::wstring& str, const std::wstring& from, const std::wstring& to)
{
    std::wstring _copy(str);
    replace_all<std::wstring>(_copy, from, to, std::wstring::npos);
    return _copy;
}
//=============================================================================
bool
replace_first(std::string& str, const std::string& from, const std::string& to)
{
    return replace_first<std::string>(str, from, to, std::string::npos);
}
//=============================================================================
bool
replace_last(std::string& str, const std::string& from, const std::string& to)
{
    return replace_last<std::string>(str, from, to, std::string::npos);
}
//=============================================================================
void
replace_all(std::string& str, const std::string& from, const std::string& to)
{
    replace_all<std::string>(str, from, to, std::string::npos);
}
//=============================================================================
void
split(std::vector<std::wstring>& result, const std::wstring& s, wchar_t separator)
{
    std::wstring::size_type prev_pos = 0, pos = 0;
    while ((pos = s.find(separator, pos)) != std::wstring::npos) {
        std::wstring substring(s.substr(prev_pos, pos - prev_pos));
        result.push_back(substring);
        prev_pos = ++pos;
    }
    result.push_back(s.substr(prev_pos, pos - prev_pos));
}
//=============================================================================
bool
iequals(const std::string& s1, const std::string& s2)
{
    if (s1.size() != s2.size()) {
        return false;
    }
    if (s1.empty() != s2.empty()) {
        return false;
    }
    return std::equal(
        s1.begin(), s1.end(), s2.begin(), [](char a, char b) { return tolower(a) == tolower(b); });
}
//=============================================================================
bool
iequals(const std::wstring& s1, const std::wstring& s2)
{
    if (s1.size() != s2.size()) {
        return false;
    }
    if (s1.empty() != s2.empty()) {
        return false;
    }
    return std::equal(s1.begin(), s1.end(), s2.begin(),
        [](wchar_t a, wchar_t b) { return tolower(a) == tolower(b); });
}
//=============================================================================
bool
str2integer(const std::wstring& str, int& value)
{
    std::wstringstream ss(str);
    if ((ss >> value).fail() || !(ss >> std::ws).eof()) {
        return false;
    }
    return true;
}
//=============================================================================
bool
str2longlong(const std::wstring& str, long long& value)
{
    std::wstringstream ss(str);
    if ((ss >> value).fail() || !(ss >> std::ws).eof()) {
        return false;
    }
    return true;
}
//=============================================================================
template <class T>
bool
contains(const T& in, const T& needle, size_t npos)
{
#ifdef __APPLE__
    return (in.find(needle) != npos);
#else
    auto it = std::search(
        in.begin(), in.end(), std::boyer_moore_searcher(needle.begin(), needle.end()));
    return (it != in.end());
#endif
}
//=============================================================================
bool
contains(const std::wstring& in, const std::wstring& needle)
{
    return contains<std::wstring>(in, needle, std::wstring::npos);
}
//=============================================================================
bool
contains(const std::string& in, const std::string& needle)
{
    return contains<std::string>(in, needle, std::string::npos);
}
//=============================================================================
bool
icontains(const std::wstring& in, const std::wstring& needle)
{
    std::wstring IN = StringHelpers::to_upper_copy(in);
    std::wstring NEEDLE = StringHelpers::to_upper_copy(needle);
    return contains<std::wstring>(IN, NEEDLE, std::wstring::npos);
}
//=============================================================================
bool
icontains(const std::string& in, const std::string& needle)
{
    std::string IN = StringHelpers::to_upper_copy(in);
    std::string NEEDLE = StringHelpers::to_upper_copy(needle);
    return contains<std::string>(IN, NEEDLE, std::string::npos);
}
//=============================================================================
static bool
isSpace(wchar_t c)
{
    return (c == L' ') || (c == L'\f') || (c == L'\n') || (c == L'\r') || (c == L'\t')
        || (c == L'\v');
}
//=============================================================================
static bool
isSpace(char c)
{
    return (c == ' ') || (c == '\f') || (c == '\n') || (c == '\r') || (c == '\t') || (c == '\v');
}
//=============================================================================
void
trim_left(std::string& in_out)
{
    if (in_out.empty()) {
        return;
    }
    in_out.erase(in_out.begin(),
        std::find_if(in_out.begin(), in_out.end(), [](char ch) { return !isSpace(ch); }));
}
//=============================================================================
void
trim_right(std::string& in_out)
{
    if (in_out.empty()) {
        return;
    }
    in_out.erase(
        std::find_if(in_out.rbegin(), in_out.rend(), [](char ch) { return !isSpace(ch); }).base(),
        in_out.end());
}
//=============================================================================
void
trim(std::string& in_out)
{
    trim_right(in_out);
    trim_left(in_out);
}
//=============================================================================
template <class T>
T
trim_copy(const T& in_out)
{
    T _copy(in_out);
    trim(_copy);
    return _copy;
}
//=============================================================================
std::wstring
trim_copy(const std::wstring& in_out)
{
    return trim_copy<std::wstring>(in_out);
}
//=============================================================================
std::string
trim_copy(const std::string& in_out)
{
    return trim_copy<std::string>(in_out);
}
//=============================================================================
void
trim_left(std::wstring& in_out)
{
    in_out.erase(in_out.begin(),
        std::find_if(in_out.begin(), in_out.end(), [](wchar_t ch) { return !isSpace(ch); }));
}
//=============================================================================
void
trim_right(std::wstring& in_out)
{
    in_out.erase(
        std::find_if(in_out.rbegin(), in_out.rend(), [](wchar_t ch) { return !isSpace(ch); })
            .base(),
        in_out.end());
}
//=============================================================================
template <class T>
T
trim_right_copy(const T& in_out)
{
    T _copy(in_out);
    trim_right(_copy);
    return _copy;
}
//=============================================================================
std::wstring
trim_right_copy(const std::wstring& in_out)
{
    return trim_right_copy<std::wstring>(in_out);
}
//=============================================================================
std::string
trim_right_copy(const std::string& in_out)
{
    return trim_right_copy<std::string>(in_out);
}
//=============================================================================
void
trim(std::wstring& in_out)
{
    trim_right(in_out);
    trim_left(in_out);
}
//=============================================================================
template <class T>
T
trim_left_copy(const T& in_out)
{
    T _copy(in_out);
    trim_left(_copy);
    return _copy;
}
//=============================================================================
std::wstring
trim_left_copy(const std::wstring& in_out)
{
    return trim_left_copy<std::wstring>(in_out);
}
//=============================================================================
std::string
trim_left_copy(const std::string& in_out)
{
    return trim_left_copy<std::string>(in_out);
}
//=============================================================================
template <class T>
void
erase_all(T& input, const T& search, size_t npos)
{
    size_t pos = npos;
    while ((pos = input.find(search)) != std::string::npos) {
        input.erase(pos, search.length());
    }
}
//=============================================================================
void
erase_all(std::string& input, const std::string& search)
{
    return erase_all<std::string>(input, search, std::string::npos);
}
//=============================================================================
void
erase_all(std::wstring& input, const std::wstring& search)
{
    return erase_all<std::wstring>(input, search, std::wstring::npos);
}
//=============================================================================
std::wstring
erase_all_copy(const std::wstring& input, const std::wstring& search)
{
    std::wstring _copy(input);
    erase_all(_copy, search);
    return _copy;
}
//=============================================================================
template <class T>
void
erase_first(T& input, const T& search, size_t npos)
{
    size_t pos = input.find(search);
    if (pos != npos) {
        input.erase(pos, search.length());
    }
}
//=============================================================================
void
erase_first(std::string& input, const std::string& search)
{
    erase_first<std::string>(input, search, std::string::npos);
}
//=============================================================================
void
erase_first(std::wstring& input, const std::wstring& search)
{
    erase_first<std::wstring>(input, search, std::string::npos);
}
//=============================================================================
std::string
join(const std::vector<std::string>& inputs, const std::string& separator)
{
    return fmt::format("{}", fmt::join(inputs, separator));
}
//=============================================================================
std::wstring
join(const std::vector<std::wstring>& inputs, const std::wstring& separator)
{
    return fmt::format(L"{}", fmt::join(inputs, separator));
}
//=============================================================================
int
count(const std::wstring& string, const std::wstring& subString)
{
    if (subString.empty()) {
        return 0;
    }
    int count = 0;
    size_t pos = string.find(subString);
    while (pos != std::wstring::npos) {
        ++count;
        pos = string.find(subString, pos + subString.length());
    }
    return count;
}
//=============================================================================
int
count(const std::string& string, const std::string& subString)
{
    if (subString.empty()) {
        return 0;
    }

    int count = 0;
    size_t pos = string.find(subString);

    while (pos != std::string::npos) {
        ++count;
        pos = string.find(subString, pos + subString.length());
    }
    return count;
}
//=============================================================================
}
//=============================================================================
