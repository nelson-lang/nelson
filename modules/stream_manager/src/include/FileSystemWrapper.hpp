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
#ifdef _MSC_VER
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <process.h>
#undef min
#undef max
#else
#include <unistd.h>
#endif
#include <filesystem>
#include <stdio.h>
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
public:
    //=============================================================================
    Path() = default;
    explicit Path(Path const& p) = default;
    //=============================================================================
    Path(const std::wstring& p)
    {
#ifdef _MSC_VER
        nativePath = p;
#else
        nativePath = wstring_to_utf8(p);
#endif
    }
    //=============================================================================
    explicit Path(const std::string& p)
    {
#ifdef _MSC_VER
        nativePath = utf8_to_wstring(p);
#else
        nativePath = p;
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
        return (Path) * this;
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
        nativePath = pr.native();
        return (Path) * this;
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
        nativePath = pr.native();
        return (Path) * this;
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
        nativePath = pr.native();
        return (Path) * this;
    }
    //=============================================================================
    Path
    operator/(const Path& p2)
    {
        std::filesystem::path res;
        std::filesystem::path pr(nativePath);
        std::filesystem::path pa(p2.nativePath);
        res = pr / pa;
        nativePath = res.native();
        return (Path) * this;
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
        nativePath = res.native();
        return (Path) * this;
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
        nativePath = res.native();
        return (Path) * this;
    }
    //=============================================================================
    [[nodiscard]] auto
    native() const
    {
        return nativePath;
    }
    //=============================================================================
    [[nodiscard]] auto
    has_filename() const
    {
        return std::filesystem::path(nativePath).has_filename();
    }
    //=============================================================================
    [[nodiscard]] auto
    has_extension() const
    {
        return std::filesystem::path(nativePath).has_extension();
    }
    //=============================================================================
    [[nodiscard]] auto
    extension() const
    {
        return Path(std::filesystem::path(nativePath).extension().native());
    }
    //=============================================================================
    [[nodiscard]] std::wstring
    wstring() const
    {
#ifdef _MSC_VER
        return nativePath;
#else
        return utf8_to_wstring(nativePath);
#endif
    }
    //=============================================================================
    [[nodiscard]] auto
    string() const
    {
#ifdef _MSC_VER
        return wstring_to_utf8(nativePath);
#else
        return nativePath;
#endif
    }
    //=============================================================================
    [[nodiscard]] auto
    generic_wstring() const
    {
#ifdef _MSC_VER
        return std::filesystem::path(nativePath).generic_wstring();
#else
        return utf8_to_wstring(std::filesystem::path(nativePath).generic_string());
#endif
    }
    //=============================================================================
    [[nodiscard]] auto
    generic_string() const
    {
#ifdef _MSC_VER
        return wstring_to_utf8(std::filesystem::path(nativePath).generic_wstring());
#else
        return std::filesystem::path(nativePath).generic_string();
#endif
    }
    //=============================================================================
    [[nodiscard]] auto
    generic_path() const
    {
#ifdef _MSC_VER
        return Path(std::filesystem::path(nativePath).generic_wstring());
#else
        return Path(std::filesystem::path(nativePath).generic_string());
#endif
    }
    //=============================================================================
    [[nodiscard]] auto
    filename() const
    {
#ifdef _MSC_VER
        return Path(std::filesystem::path(nativePath).filename().wstring());
#else
        return Path(std::filesystem::path(nativePath).filename().string());
#endif
    }
    //=============================================================================
    [[nodiscard]] auto
    leaf() const
    {
        return Path(std::filesystem::path(nativePath)).filename();
    }
    //=============================================================================
    [[nodiscard]] auto
    parent_path() const
    {
        std::filesystem::path p(nativePath);
        return Path(p.parent_path().native());
    }
    //=============================================================================
    [[nodiscard]] auto
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
    [[nodiscard]] bool
    exists() const
    {
        std::filesystem::path p1(nativePath);
        return std::filesystem::exists(p1);
    }
    //=============================================================================
    [[nodiscard]] bool
    is_directory() const
    {
        std::filesystem::path p1(nativePath);
        return std::filesystem::is_directory(p1);
    }
    //=============================================================================
    [[nodiscard]] bool
    is_regular_file() const
    {
        std::filesystem::path p1(nativePath);
        return std::filesystem::is_regular_file(p1);
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
        return (Path)result;
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
    static Path
    unique_path()
    {
        /*
        boost::filesystem::path pwd = boost::filesystem::temp_directory_path();
        pwd /= boost::filesystem::unique_path();
        return Path(pwd.native());
        */

        std::filesystem::path tempFilePath;
        std::string filetemp;
        tempFilePath = std::filesystem::temp_directory_path();

        int rd1 = rand() % 100000;
        srand(rd1);
        int rd2 = rand() % 100000;
        srand(rd2);
        int rd3 = rand() % 100000;
        srand(rd3);
        int rd4 = rand() % 100000;

#ifdef _MSC_VER
#define TMP_NELSON L"%06X-%06X-%06X-%06X-%06X.ntmp"
        wchar_t buffer[512];
        swprintf_s(buffer, 512, TMP_NELSON, _getpid(), rd1, rd2, rd3, rd4);
        tempFilePath /= std::wstring(buffer);
#else
#define TMP_NELSON "%06X-%06X-%06X-%06X-%06X.ntmp"
        char buffer[512];
        sprintf(buffer, TMP_NELSON, (int)getpid(), rd1, rd2, rd3, rd4);
        tempFilePath /= std::string(buffer);
#endif
        if (std::filesystem::exists(tempFilePath)) {
            return unique_path();
        }
        return Path(tempFilePath.native());
    }
    //=============================================================================
    static inline std::wstring
    getFinalPathname(const std::wstring& path)
    {
#ifdef _MSC_VER
#define BUFSIZE MAX_PATH
        HANDLE hFile = CreateFile(path.c_str(), GENERIC_READ, FILE_SHARE_READ | FILE_SHARE_WRITE,
            NULL, OPEN_EXISTING, FILE_FLAG_BACKUP_SEMANTICS, NULL);

        if (hFile == INVALID_HANDLE_VALUE) {
            return path;
        }
        wchar_t Path[BUFSIZE];
        DWORD dwRet = GetFinalPathNameByHandle(hFile, Path, BUFSIZE, VOLUME_NAME_DOS);
        CloseHandle(hFile);
        return std::wstring(Path).substr(4);
#endif
        return path;
    }
    //=============================================================================
    Path
    getFinalPathname()
    {
#ifdef _MSC_VER
        return Path(getFinalPathname(nativePath));
#endif
        return Path(nativePath);
    }
    //=============================================================================
};
//=============================================================================
}
//=============================================================================
