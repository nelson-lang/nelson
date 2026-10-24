// Copyright (c) 2014-2022 Thomas Fussell
// Copyright (c) 2024-2026 xlnt-community
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
// THE SOFTWARE
//
// @license: http://www.opensource.org/licenses/mit-license.php
// @author: see AUTHORS file

#include <algorithm>
#include <cassert>
#include <fstream>
#include <iterator>
#include <sstream>
#include <sys/stat.h>

#ifdef __APPLE__
#include <mach-o/dyld.h>
#elif defined(__linux)
#include <linux/limits.h>
#include <sys/types.h>
#include <unistd.h>
#endif

#include <xlnt/utils/path.hpp>
#include <xlnt/utils/string_helpers.hpp>
#include <detail/external/include_windows.hpp>

namespace {

#ifdef WIN32

char
system_separator()
{
    return '\\';
}

bool
is_drive_letter(char letter)
{
    return letter >= 'A' && letter <= 'Z';
}

bool
is_root(const std::string& part)
{
    if (part.size() == 1 && part[0] == '/') {
        return true;
    }
    if (part.size() != 3) {
        return false;
    }

    return is_drive_letter(part[0]) && part[1] == ':' && (part[2] == '\\' || part[2] == '/');
}

bool
is_absolute(const std::string& part)
{
    if (!part.empty() && part[0] == '/') {
        return true;
    }
    if (part.size() < 3) {
        return false;
    }

    return is_root(part.substr(0, 3));
}

#else

char
system_separator()
{
    return '/';
}

bool
is_root(const std::string& part)
{
    return part == "/";
}

bool
is_absolute(const std::string& part)
{
    return !part.empty() && part[0] == '/';
}

#endif

std::vector<std::string>
split_path(const std::string& path, char delim)
{
    std::vector<std::string> split;
    std::string::size_type previous_index = 0;
    auto separator_index = path.find(delim);

    while (separator_index != std::string::npos) {
        auto part = path.substr(previous_index, separator_index - previous_index);
        split.push_back(part);

        previous_index = separator_index + 1;
        separator_index = path.find(delim, previous_index);
    }

    // Don't add trailing slash
    if (previous_index < path.size()) {
        split.push_back(path.substr(previous_index));
    }

    return split;
}

#ifdef _MSC_VER
bool
file_exists(const std::wstring& path)
{
    struct _stat info;
    return _wstat(path.c_str(), &info) == 0 && (info.st_mode & S_IFREG);
}

bool
directory_exists(const std::wstring& path)
{
    struct _stat info;
    return _wstat(path.c_str(), &info) == 0 && (info.st_mode & S_IFDIR);
}
#else
bool
file_exists(const std::string& path)
{
    struct stat info;
    return stat(path.c_str(), &info) == 0 && (info.st_mode & S_IFREG);
}

bool
directory_exists(const std::string& path)
{
    struct stat info;
    return stat(path.c_str(), &info) == 0 && (info.st_mode & S_IFDIR);
}
#endif

} // namespace

namespace xlnt {

char
path::system_separator()
{
    return ::system_separator();
}

path::path() { }

path::path(const std::string& path_string)
{
    std::remove_copy(path_string.begin(), path_string.end(), std::back_inserter(internal_), '\"');
}

path::path(const std::string& path_string, char sep) : internal_(path_string)
{
    char curr_sep = guess_separator();
    if (curr_sep != sep) {
        for (char& c : internal_) // simple find and replace
        {
            if (c == curr_sep) {
                c = sep;
            }
        }
    }
}

#if XLNT_HAS_FEATURE(U8_STRING_VIEW)
path::path(std::u8string_view path_string) : path(to_string(path_string)) { }

path::path(std::u8string_view path_string, char sep) : path(to_string(path_string), sep) { }
#endif

// general attributes

bool
path::is_relative() const
{
    return !is_absolute();
}

bool
path::is_absolute() const
{
    return ::is_absolute(internal_);
}

bool
path::is_root() const
{
    return ::is_root(internal_);
}

path
path::parent() const
{
    if (is_root()) {
        return *this;
    }

    auto split_path = split();

    split_path.pop_back();

    if (split_path.empty()) {
        return path("");
    }

    path result;

    for (const auto& component : split_path) {
        result = result.append(component);
    }

    return result;
}

std::string
path::filename() const
{
    auto split_path = split();
    return split_path.empty() ? "" : split_path.back();
}

std::string
path::extension() const
{
    auto base = filename();
    auto last_dot = base.find_last_of('.');

    return last_dot == std::string::npos ? "" : base.substr(last_dot + 1);
}

std::pair<std::string, std::string>
path::split_extension() const
{
    auto base = filename();
    auto last_dot = base.find_last_of('.');

    return { base.substr(0, last_dot), base.substr(last_dot + 1) };
}

// conversion

const std::string&
path::string() const
{
    return internal_;
}

#ifdef _MSC_VER
std::wstring
path::wstring() const
{
    const std::string& path_str = string();
    const int allocated_size = MultiByteToWideChar(CP_UTF8, MB_ERR_INVALID_CHARS, path_str.c_str(),
        static_cast<int>(path_str.length()), nullptr, 0);

    if (allocated_size > 0) {
        std::wstring path_converted(allocated_size, L'\0');
        const int actual_size = MultiByteToWideChar(CP_UTF8, MB_ERR_INVALID_CHARS, path_str.c_str(),
            static_cast<int>(path_str.length()), &path_converted.at(0), allocated_size);
        assert(allocated_size
            == actual_size); // unless a serious error happened, this MUST always be true!
        return path_converted;
    } else {
        return {};
    }
}
#endif

std::vector<std::string>
path::split() const
{
    return split_path(internal_, guess_separator());
}

path
path::resolve(const path& base_path) const
{
    if (is_absolute()) {
        return *this;
    }

    path copy(base_path.internal_);

    for (const auto& part : split()) {
        if (part == "..") {
            copy = copy.parent();
            continue;
        }

        copy = copy.append(part);
    }

    return copy;
}

// filesystem attributes

bool
path::exists() const
{
    return is_file() || is_directory();
}

bool
path::is_directory() const
{
#ifdef _MSC_VER
    return directory_exists(wstring());
#else
    return directory_exists(string());
#endif
}

bool
path::is_file() const
{
#ifdef _MSC_VER
    return file_exists(wstring());
#else
    return file_exists(string());
#endif
}

// filesystem

std::string
path::read_contents() const
{
#ifdef _MSC_VER
    std::ifstream f(wstring());
#else
    std::ifstream f(string());
#endif
    std::ostringstream ss;
    ss << f.rdbuf();

    return ss.str();
}

// append

path
path::append(const std::string& to_append) const
{
    path copy(internal_);

    if (!internal_.empty() && internal_.back() != guess_separator()) {
        copy.internal_.push_back(guess_separator());
    }

    copy.internal_.append(to_append);

    return copy;
}

#if XLNT_HAS_FEATURE(U8_STRING_VIEW)
path
path::append(std::u8string_view to_append) const
{
    return append(to_string(to_append));
}
#endif

path
path::append(const path& to_append) const
{
    path copy(internal_);

    for (const auto& component : to_append.split()) {
        copy = copy.append(component);
    }

    return copy;
}

char
path::guess_separator() const
{
    if (system_separator() == '/' || internal_.empty() || internal_.front() == '/') {
        return '/';
    }
    if (is_absolute()) {
        return internal_.at(2);
    }
    return internal_.find('\\') != std::string::npos ? '\\' : '/';
}

path
path::relative_to(const path& base_path) const
{
    if (is_relative()) {
        return *this;
    }

    auto base_split = base_path.split();
    auto this_split = split();
    auto index = std::size_t(0);

    while (index < base_split.size() && index < this_split.size()
        && base_split[index] == this_split[index]) {
        index++;
    }

    auto result = path();

    for (auto i = index; i < this_split.size(); i++) {
        result = result.append(this_split[i]);
    }

    return result;
}

bool
path::operator==(const path& other) const
{
    return internal_ == other.internal_;
}

bool
path::operator!=(const path& other) const
{
    return !operator==(other);
}

} // namespace xlnt
