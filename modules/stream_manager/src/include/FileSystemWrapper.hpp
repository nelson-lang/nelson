//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include <filesystem>
#include <string>
#include <cstring>
#include <cctype>
//=============================================================================
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson::FileSystemWrapper {
//=============================================================================
class Path
{
    //=============================================================================
private:
#ifdef _MSC_VER
    std::wstring nativePath;
#else
    std::string nativePath;
#endif
    //=============================================================================
    static Path
    removeLastSeparator(const Path& p)
    {
#ifdef _MSC_VER
        std::wstring nativeString = p.nativePath;
        if (nativeString.length() > 1
            && (nativeString.back() == L'/' || nativeString.back() == L'\\')) {
            nativeString.pop_back();
        }
#else
        std::string nativeString = p.nativePath;
        if (nativeString.length() > 1
            && (nativeString.back() == '/' || nativeString.back() == '\\')) {
            nativeString.pop_back();
        }
#endif
        return Path(nativeString);
    }
    //=============================================================================
    std::wstring
    uppercaseDriveLetter(const std::wstring& path)
    {
        std::wstring _path(path);
        if (_path.length() > 1 && _path[1] == L':') {
            _path[0] = ::towupper(_path[0]);
        }
        return _path;
    }
    //=============================================================================
    std::string
    uppercaseDriveLetter(const std::string& path)
    {
        std::string _path(path);
        if (_path.length() > 1 && _path[1] == ':') {
            _path[0] = ::toupper(_path[0]);
        }
        return _path;
    }
    //=============================================================================
public:
    //=============================================================================
    Path() { }
    Path(Path const& p) : nativePath(p.nativePath) { }
    //=============================================================================
    Path(const std::wstring& p)
    {
#ifdef _MSC_VER
        nativePath = uppercaseDriveLetter(p);
#else
        std::string native = wstring_to_utf8(p);
        nativePath = uppercaseDriveLetter(native);
#endif
    }
    //=============================================================================
    Path(const std::string& p)
    {
#ifdef _MSC_VER
        std::wstring native = utf8_to_wstring(p);
        nativePath = uppercaseDriveLetter(native);
#else
        nativePath = uppercaseDriveLetter(p);
#endif
    }
    //=============================================================================
    Path&
    operator=(Path& p)
    {
        nativePath = std::move(p.nativePath);
        return *this;
    }
    //=============================================================================
    Path&
    assign(Path const& p)
    {
        nativePath = p.nativePath;
        return *this;
    }
    //=============================================================================
    Path&
    operator=(Path const& p)
    {
        return Path::assign(p);
    }
    //=============================================================================
    Path&
    operator=(const std::wstring& p)
    {
        nativePath = Path(p).nativePath;
        return *this;
    }
    //=============================================================================
    Path
    operator=(const std::string& p)
    {
        nativePath = Path(p).nativePath;
        return *this;
    }
    //=============================================================================
    Path&
    concat(Path const& p)
    {
        nativePath += p.nativePath;
        return *this;
    }
    //=============================================================================
    Path&
    concat(const std::wstring& s)
    {
#ifdef _MSC_VER
        nativePath += s;
#else
        nativePath += wstring_to_utf8(s);
#endif
        return *this;
    }
    //=============================================================================
    Path&
    concat(const std::string& s)
    {
#ifdef _MSC_VER
        nativePath += utf8_to_wstring(s);
#else
        nativePath += s;
#endif
        return *this;
    }
    //=============================================================================
    Path&
    operator+=(const std::string& s)
    {
        return concat(s);
    }
    //=============================================================================
    Path&
    operator+=(const std::wstring& s)
    {
        return concat(s);
    }
    //=============================================================================
    Path
    operator/=(Path const& p)
    {
        std::filesystem::path pr(nativePath);
        std::filesystem::path pa(p.nativePath);
        pr /= pa;
        return Path(pr.native());
    }
    //=============================================================================
    Path
    operator/=(const std::wstring& s)
    {
        std::filesystem::path pr(nativePath);
#ifdef _MSC_VER
        std::filesystem::path pa(s);
#else
        std::filesystem::path pa(wstring_to_utf8(s));
#endif
        pr /= pa;
        return Path(pr.native());
    }
    //=============================================================================
    Path
    operator/=(const std::string& s)
    {
        std::filesystem::path pr(nativePath);
#ifdef _MSC_VER
        std::filesystem::path pa(utf8_to_wstring(s));
#else
        std::filesystem::path pa(s);
#endif
        pr /= pa;
        return Path(pr.native());
    }
    //=============================================================================
    Path
    operator/(const Path& p2)
    {
        std::filesystem::path res;
        std::filesystem::path pr(nativePath);
        std::filesystem::path pa(p2.nativePath);
        res = pr / pa;
        return Path(res.native());
    }
    //=============================================================================
    Path
    operator/(const std::string& p2)
    {
        std::filesystem::path res;
        std::filesystem::path pr(nativePath);
#ifdef _MSC_VER
        std::filesystem::path pa(utf8_to_wstring(p2));
#else
        std::filesystem::path pa(p2);
#endif
        res = pr / pa;
        return Path(res.native());
    }
    //=============================================================================
    Path
    operator/(const std::wstring& p2)
    {
        std::filesystem::path res;
        std::filesystem::path pr(nativePath);
#ifdef _MSC_VER
        std::filesystem::path pa(p2);
#else
        std::filesystem::path pa(wstring_to_utf8(p2));
#endif
        res = pr / pa;
        return Path(res.native());
    }
    //=============================================================================
    auto
    native() const
    {
        return nativePath;
    }
    //=============================================================================
    auto
    has_filename() const
    {
        return std::filesystem::path(nativePath).has_filename();
    }
    //=============================================================================
    auto
    has_extension() const
    {
        return std::filesystem::path(nativePath).has_extension();
    }
    //=============================================================================
    auto
    extension() const
    {
        return Path(std::filesystem::path(nativePath).extension().native());
    }
    //=============================================================================
    auto
    wstring() const
    {
#ifdef _MSC_VER
        return nativePath;
#else
        return utf8_to_wstring(nativePath);
#endif
    }
    //=============================================================================
    auto
    string() const
    {
#ifdef _MSC_VER
        return wstring_to_utf8(nativePath);
#else
        return nativePath;
#endif
    }
    //=============================================================================
    auto
    generic_wstring() const
    {
#ifdef _MSC_VER
        return std::filesystem::path(nativePath).generic_wstring();
#else
        return utf8_to_wstring(std::filesystem::path(nativePath).generic_string());
#endif
    }
    //=============================================================================
    auto
    generic_string() const
    {
#ifdef _MSC_VER
        return wstring_to_utf8(std::filesystem::path(nativePath).generic_wstring());
#else
        return std::filesystem::path(nativePath).generic_string();
#endif
    }
    //=============================================================================
    auto
    generic_path() const
    {
#ifdef _MSC_VER
        return Path(std::filesystem::path(nativePath).generic_wstring());
#else
        return Path(std::filesystem::path(nativePath).generic_string());
#endif
    }
    //=============================================================================
    auto
    filename() const
    {
#ifdef _MSC_VER
        return Path(std::filesystem::path(nativePath).filename().wstring());
#else
        return Path(std::filesystem::path(nativePath).filename().string());
#endif
    }
    //=============================================================================
    auto
    leaf() const
    {
        return Path(std::filesystem::path(nativePath)).filename();
    }
    //=============================================================================
    auto
    parent_path() const
    {
        std::filesystem::path p(nativePath);
        return Path(p.parent_path().native());
    }
    //=============================================================================
    auto
    stem() const
    {
        std::filesystem::path p(nativePath);
        return Path(p.stem().native());
    }
    //=============================================================================
    auto
    replace_extension(Path const& new_extension = Path())
    {
        std::filesystem::path p1(nativePath);
        std::filesystem::path ext(new_extension.native());
        return Path(p1.replace_extension(ext).native());
    }
    //=============================================================================
    auto
    has_parent_path()
    {
        std::filesystem::path p1(nativePath);
        return p1.has_parent_path();
    }
    //=============================================================================
    bool
    exists() const
    {
        std::filesystem::path p1(nativePath);
        return std::filesystem::exists(p1);
    }
    //=============================================================================
    bool
    is_directory() const
    {
        std::filesystem::path p1(nativePath);
        return std::filesystem::is_directory(p1);
    }
    //=============================================================================
    auto
    normalize()
    {
        std::filesystem::path p1(nativePath);
        return Path(p1.lexically_normal().native());
    }
    //=============================================================================
    static bool
    exists(Path const& p)
    {
        std::filesystem::path p1(p.nativePath);
        return std::filesystem::exists(p1);
    }
    //=============================================================================
    static bool
    is_directory(Path const& p)
    {
        std::filesystem::path p1(p.nativePath);
        try {
            return std::filesystem::is_directory(p1);
        } catch (const std::filesystem::filesystem_error&) {
        }
        return false;
    }
    //=============================================================================
    static auto
    canonical(Path const& p1, std::string& errorMessage)
    {
        std::filesystem::path p(p1.nativePath);
        try {
            return Path(std::filesystem::canonical(p).native());
        } catch (const std::filesystem::filesystem_error& e) {
            std::error_code error_code = e.code();
            errorMessage = error_code.message();
        }
        return Path();
    }
    //=============================================================================
    static bool
    equivalent(Path const& p1, Path const& p2, std::string& errorMessage)
    {
        std::filesystem::path _p1(p1.nativePath);
        std::filesystem::path _p2(p2.nativePath);
        try {
            return std::filesystem::equivalent(_p1, _p2);
        } catch (const std::filesystem::filesystem_error& e) {
            std::error_code error_code = e.code();
            errorMessage = error_code.message();
        }
        return false;
    }
    //=============================================================================
    static bool
    equivalent(Path const& p1, Path const& p2)
    {
        std::string errorMessage;
        return equivalent(p1, p2, errorMessage);
    }
    //=============================================================================
    static bool
    remove(Path const& p, std::string& errorMessage)
    {
        bool bRes = false;
        try {
            bRes = std::filesystem::remove(std::filesystem::path(p.nativePath));
            errorMessage.clear();
        } catch (const std::filesystem::filesystem_error& e) {
            std::error_code error_code = e.code();
            bRes = false;
            errorMessage = error_code.message();
        }
        return bRes;
    }
    //=============================================================================
    static bool
    remove(Path const& p)
    {
        std::string message;
        return remove(p, message);
    }
    //=============================================================================
    static bool
    remove_all(Path const& p, std::string& errorMessage)
    {
        bool bRes = false;
        try {
            bRes = std::filesystem::remove_all(std::filesystem::path(p.nativePath));
        } catch (const std::filesystem::filesystem_error& e) {
            bRes = false;
            std::error_code error_code = e.code();
            bRes = false;
            errorMessage = error_code.message();
        }
        return bRes;
    }
    //=============================================================================
    static bool
    remove_all(Path const& p)
    {
        std::string message;
        return remove_all(p, message);
    }
    //=============================================================================
    auto
    is_absolute()
    {
        return std::filesystem::path(nativePath).is_absolute();
    }
    //=============================================================================
    static Path
    absolute(Path const& p, std::string& errorMessage)
    {
        Path result;
        std::filesystem::path _p(p.nativePath);
        try {
            result = Path(std::filesystem::absolute(_p).native());
        } catch (const std::filesystem::filesystem_error& e) {
            std::error_code error_code = e.code();
            errorMessage = error_code.message();
            result = p;
        }
        return result;
    }
    //=============================================================================
    static Path
    absolute(Path const& p)
    {
        std::string errorMessage;
        return absolute(p, errorMessage);
    }
    //=============================================================================
    static auto
    copy_file(Path const& p1, Path const& p2, std::string& errorMessage)
    {
        bool bRes = false;
        std::filesystem::path _p1(p1.nativePath);
        std::filesystem::path _p2(p2.nativePath);
        try {
            bRes = std::filesystem::copy_file(
                _p1, _p2, std::filesystem::copy_options::overwrite_existing);
        } catch (const std::filesystem::filesystem_error& e) {
            std::error_code error_code = e.code();
            errorMessage = error_code.message();
            bRes = false;
        }
        return bRes;
    }
    //=============================================================================
    static auto
    copy_file(Path const& p1, Path const& p2)
    {
        std::string errorMessage;
        return copy_file(p1, p2, errorMessage);
    }
    //=============================================================================
    static auto
    copy(Path const& from, Path const& to, std::string& errorMessage)
    {
        bool bRes = false;
        std::filesystem::path _from(from.nativePath);
        std::filesystem::path _to(to.nativePath);
        try {
            std::filesystem::copy(_from, _to);
            bRes = true;
        } catch (const std::filesystem::filesystem_error& e) {
            std::error_code error_code = e.code();
            errorMessage = error_code.message();
            bRes = false;
        }
        return bRes;
    }
    //=============================================================================
    static auto
    copy(Path const& from, Path const& to)
    {
        std::string errorMessage;
        return copy(from, to, errorMessage);
    }
    //=============================================================================
    static auto
    create_directories(const Path& p, std::string& errorMessage)
    {
        std::error_code error_code;
        Path pp = removeLastSeparator(p);
        bool res
            = std::filesystem::create_directories(std::filesystem::path(pp.nativePath), error_code);
        errorMessage = error_code.message();
        return res;
    }
    //=============================================================================
    static auto
    create_directories(const Path& p, bool& permissionDenied)
    {
        std::error_code error_code;
        Path pp = removeLastSeparator(p);
        bool res
            = std::filesystem::create_directories(std::filesystem::path(pp.nativePath), error_code);
        if (error_code == std::errc::permission_denied) {
            permissionDenied = true;
        }
        return res;
    }
    //=============================================================================
    static auto
    create_directories(const Path& p)
    {
        bool permissionDenied;
        return create_directories(p, permissionDenied);
    }
    //=============================================================================
    static auto
    create_directories(const std::wstring& wstr)
    {
        bool permissionDenied;
        return create_directories(Path(wstr), permissionDenied);
    }
    //=============================================================================
    static auto
    create_directory(const Path& p, bool& permissionDenied)
    {
        bool res = false;
        try {
            Path pp = removeLastSeparator(p);
            res = std::filesystem::create_directory(std::filesystem::path(pp.nativePath));
        } catch (const std::filesystem::filesystem_error& e) {
            res = false;
            if (e.code() == std::errc::permission_denied) {
                permissionDenied = true;
            }
        }
        return res;
    }
    //=============================================================================
    static bool
    create_directory(const Path& p, std::string& errorMessage)
    {
        bool res = false;
        try {
            Path pp = removeLastSeparator(p);
            res = std::filesystem::create_directory(std::filesystem::path(pp.nativePath));
        } catch (const std::filesystem::filesystem_error& e) {
            res = false;
            std::error_code error_code = e.code();
            errorMessage = error_code.message();
        }
        return res;
    }
    //=============================================================================
    static auto
    create_directory(const Path& p)
    {
        bool permissionDenied;
        return create_directory(p, permissionDenied);
    }
    //=============================================================================
    static auto
    file_size(const Path& p, std::string& errorMessage)
    {
        std::uintmax_t value = 0;
        try {
            value = std::filesystem::file_size(std::filesystem::path(p.nativePath));
        } catch (const std::filesystem::filesystem_error& e) {
            value = 0;
            std::error_code error_code = e.code();
            errorMessage = error_code.message();
        }
        return value;
    }
    //=============================================================================
    static auto
    file_size(const Path& p)
    {
        std::string errorMessage;
        return file_size(p, errorMessage);
    }
    //=============================================================================
    template <typename TP>
    static std::time_t
    to_time_t(TP tp)
    {
        using namespace std::chrono;
        auto sctp
            = time_point_cast<system_clock::duration>(tp - TP::clock::now() + system_clock::now());
        return system_clock::to_time_t(sctp);
    }
    //=============================================================================
    static auto
    last_write_time(const Path& p, std::string& errorMessage)
    {
        std::filesystem::file_time_type file_time;
        errorMessage.clear();
        try {
            file_time = std::filesystem::last_write_time(std::filesystem::path(p.nativePath));
        } catch (const std::filesystem::filesystem_error& e) {
            std::error_code error_code = e.code();
            errorMessage = error_code.message();
        }
        std::time_t result = to_time_t(file_time);
        return result;
    }
    //=============================================================================
    static auto
    last_write_time(const Path& p)
    {
        std::string errorMessage;
        return last_write_time(p, errorMessage);
    }
    //=============================================================================
    auto
    lexically_normal()
    {
        std::filesystem::path p(nativePath);
        return Path(p.lexically_normal().native());
    }
    //=============================================================================
    auto
    lexically_relative(const Path& p)
    {
        std::filesystem::path _p1(nativePath);
        std::filesystem::path _p2(p.nativePath);
        return Path(_p1.lexically_relative(_p2).native());
    }
    //=============================================================================
    static auto
    current_path()
    {
        try {
            return Path(std::filesystem::current_path().native());
        } catch (const std::filesystem::filesystem_error&) {
        }
        return Path();
    }
    //=============================================================================
    static void
    current_path(Path const& p, std::string& errorMessage)
    {
        try {
            std::filesystem::current_path(std::filesystem::path(p.nativePath));
        } catch (const std::filesystem::filesystem_error& e) {
            std::error_code error_code = e.code();
            errorMessage = error_code.message();
        }
    }
    //=============================================================================
    static auto
    current_path(Path const& p)
    {
        std::string errorMessage;
        return current_path(p, errorMessage);
    }
    //=============================================================================
    static auto
    current_path(const std::wstring& wstr)
    {
        std::string errorMessage;
        return current_path(Path(wstr), errorMessage);
    }
    //=============================================================================
    auto
    parent_path()
    {
        std::filesystem::path p(nativePath);
        return Path(p.parent_path().native());
    }
    //=============================================================================
    static auto
    temp_directory_path()
    {
        return Path(std::filesystem::temp_directory_path().native());
    }
    //=============================================================================
    static auto
    unique_path()
    {
        std::filesystem::path tempFilePath = std::filesystem::temp_directory_path();
#ifdef _MSC_VER
        char name[L_tmpnam_s];
        tmpnam_s(name, L_tmpnam_s);
        tempFilePath /= std::string(name);
#else
        FILE* uid_file;
        std::string template_name = tempFilePath.string() + "/NELSON_XXXXXX";
        char template_char[FILENAME_MAX * 2];
        strncpy(template_char, template_name.c_str(), template_name.length());
        int temp_fd = mkstemp(template_char);
        uid_file = fdopen(temp_fd, "w");
        if (uid_file) {
            tempFilePath = std::string(template_char);
            fclose(uid_file);
        } else {
            tempFilePath /= "/NELSON_XXXXXX";
        }
#endif
        return Path(tempFilePath.native());
    }
    //=============================================================================
};
//=============================================================================
}
//=============================================================================
