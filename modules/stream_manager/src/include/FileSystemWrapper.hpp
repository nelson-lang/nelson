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
#define _WITH_BOOST_FILESYSTEM_
#else
#undef _WITH_BOOST_FILESYSTEM_
#endif
//=============================================================================
#ifdef _WITH_BOOST_FILESYSTEM_
#include <boost/filesystem.hpp>
#include <boost/filesystem/path.hpp>
#else
#include <atomic>
#include <filesystem>
#include <stdio.h>
#include <string>
#include <cstring>
#include <cctype>
#endif
#ifdef _MSC_VER
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <process.h>
#undef min
#undef max
#else
#include <unistd.h>
#endif
//=============================================================================
#include "characters_encoding.hpp"
//=============================================================================
#ifdef _WITH_BOOST_FILESYSTEM_
namespace nfs = boost::filesystem;
#else
namespace nfs = std::filesystem;
#endif
//=============================================================================
namespace Nelson::FileSystemWrapper {
//=============================================================================
class Path
{
    //=============================================================================
private:
    //=============================================================================
    static int
    generateUid()
    {
        static std::atomic<int> uid { 0 };
        return ++uid;
    }
    //=============================================================================
    static auto
    getUniqueID()
    {
        int uid = generateUid();
        srand(uid);
        int rd1 = rand() % 100000;
        srand(uid + rd1);
        int rd2 = rand() % 100000;
        srand(uid + rd2);
        int rd3 = rand() % 100000;
        srand(uid + rd3);
        int rd4 = rand() % 100000;
#ifdef _MSC_VER
#define TMP_NELSON L"%06x-%06x-%06x-%06x-%06x-%06x.tmp"
        wchar_t buffer[512];
        swprintf_s(buffer, 512, TMP_NELSON, _getpid(), rd1, rd2, rd3, rd4, uid);
        return std::wstring(buffer);
#else
#define TMP_NELSON "%06x-%06x-%06x-%06x-%06x-%06x.tmp"
        char buffer[512];
        sprintf(buffer, TMP_NELSON, (int)getpid(), rd1, rd2, rd3, rd4, uid);
        return std::string(buffer);
#endif
    }
    //=============================================================================
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
        nfs::path pr(nativePath);
        nfs::path pa(p.nativePath);
        pr /= pa;
        nativePath = pr.native();
        return (Path) * this;
    }
    //=============================================================================
    Path
    operator/=(const std::wstring& s)
    {
        nfs::path pr(nativePath);
#ifdef _MSC_VER
        nfs::path pa(s);
#else
        nfs::path pa(wstring_to_utf8(s));
#endif
        pr /= pa;
        nativePath = pr.native();
        return (Path) * this;
    }
    //=============================================================================
    Path
    operator/=(const std::string& s)
    {
        nfs::path pr(nativePath);
#ifdef _MSC_VER
        nfs::path pa(utf8_to_wstring(s));
#else
        nfs::path pa(s);
#endif
        pr /= pa;
        nativePath = pr.native();
        return (Path) * this;
    }
    //=============================================================================
    Path
    operator/(const Path& p2)
    {
        nfs::path res;
        nfs::path pr(nativePath);
        nfs::path pa(p2.nativePath);
        res = pr / pa;
        nativePath = res.native();
        return (Path) * this;
    }
    //=============================================================================
    Path
    operator/(const std::string& p2)
    {
        nfs::path res;
        nfs::path pr(nativePath);
#ifdef _MSC_VER
        nfs::path pa(utf8_to_wstring(p2));
#else
        nfs::path pa(p2);
#endif
        res = pr / pa;
        nativePath = res.native();
        return (Path) * this;
    }
    //=============================================================================
    Path
    operator/(const std::wstring& p2)
    {
        nfs::path res;
        nfs::path pr(nativePath);
#ifdef _MSC_VER
        nfs::path pa(p2);
#else
        nfs::path pa(wstring_to_utf8(p2));
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
        return nfs::path(nativePath).has_filename();
    }
    //=============================================================================
    [[nodiscard]] auto
    has_extension() const
    {
        return nfs::path(nativePath).has_extension();
    }
    //=============================================================================
    [[nodiscard]] auto
    extension() const
    {
        return Path(nfs::path(nativePath).extension().native());
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
        return nfs::path(nativePath).generic_wstring();
#else
        return utf8_to_wstring(nfs::path(nativePath).generic_string());
#endif
    }
    //=============================================================================
    [[nodiscard]] auto
    generic_string() const
    {
#ifdef _MSC_VER
        return wstring_to_utf8(nfs::path(nativePath).generic_wstring());
#else
        return nfs::path(nativePath).generic_string();
#endif
    }
    //=============================================================================
    [[nodiscard]] auto
    generic_path() const
    {
#ifdef _MSC_VER
        return Path(nfs::path(nativePath).generic_wstring());
#else
        return Path(nfs::path(nativePath).generic_string());
#endif
    }
    //=============================================================================
    [[nodiscard]] auto
    filename() const
    {
#ifdef _MSC_VER
        return Path(nfs::path(nativePath).filename().wstring());
#else
        return Path(nfs::path(nativePath).filename().string());
#endif
    }
    //=============================================================================
    [[nodiscard]] auto
    leaf() const
    {
        return Path(nfs::path(nativePath).native()).filename();
    }
    //=============================================================================
    [[nodiscard]] auto
    parent_path() const
    {
        nfs::path p(nativePath);
        return Path(p.parent_path().native());
    }
    //=============================================================================
    [[nodiscard]] auto
    stem() const
    {
        nfs::path p(nativePath);
        return Path(p.stem().native());
    }
    //=============================================================================
    auto
    replace_extension(Path const& new_extension = Path())
    {
        nfs::path p1(nativePath);
        nfs::path ext(new_extension.native());
        return Path(p1.replace_extension(ext).native());
    }
    //=============================================================================
    auto
    has_parent_path()
    {
        nfs::path p1(nativePath);
        return p1.has_parent_path();
    }
    //=============================================================================
    [[nodiscard]] bool
    exists(std::string& errorMessage) const
    {
        nfs::path p1(nativePath);
        try {
            return nfs::exists(p1);
        } catch (const nfs::filesystem_error& e) {
            errorMessage = e.what();
        }
        return false;
    }
    //=============================================================================
    [[nodiscard]] bool
    exists() const
    {
        std::string errorMessage;
        return exists(errorMessage);
    }
    //=============================================================================
    static bool
    exists(Path const& p)
    {
        return p.exists();
    }
    //=============================================================================
    [[nodiscard]] bool
    is_directory() const
    {
        nfs::path p1(nativePath);
        return nfs::is_directory(p1);
    }
    //=============================================================================
    [[nodiscard]] bool
    is_regular_file() const
    {
        nfs::path p1(nativePath);
        return nfs::is_regular_file(p1);
    }
    //=============================================================================
    auto
    normalize()
    {
        nfs::path p1(nativePath);
        return Path(p1.lexically_normal().native());
    }
    //=============================================================================
    static bool
    is_directory(Path const& p)
    {
        nfs::path p1(p.nativePath);
        try {
            return nfs::is_directory(p1);
        } catch (const nfs::filesystem_error&) {
        }
        return false;
    }
    //=============================================================================
    static auto
    canonical(Path const& p1, std::string& errorMessage)
    {
        nfs::path p(p1.nativePath);
        try {
            return Path(nfs::canonical(p).native());
        } catch (const nfs::filesystem_error& e) {
            std::error_code error_code = e.code();
            errorMessage = error_code.message();
        }
        return Path();
    }
    //=============================================================================
    static bool
    equivalent(Path const& p1, Path const& p2, std::string& errorMessage)
    {
        nfs::path _p1(p1.nativePath);
        nfs::path _p2(p2.nativePath);
        try {
            return nfs::equivalent(_p1, _p2);
        } catch (const nfs::filesystem_error& e) {
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
            bRes = nfs::remove(nfs::path(p.nativePath));
            errorMessage.clear();
        } catch (const nfs::filesystem_error& e) {
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
            bRes = nfs::remove_all(nfs::path(p.nativePath));
        } catch (const nfs::filesystem_error& e) {
            std::error_code error_code = e.code();
            errorMessage = error_code.message();
            bRes = false;
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
        return nfs::path(nativePath).is_absolute();
    }
    //=============================================================================
    static Path
    absolute(Path const& p, std::string& errorMessage)
    {
        Path result;
        nfs::path _p(p.nativePath);
        try {
            result = Path(nfs::absolute(_p).native());
        } catch (const nfs::filesystem_error& e) {
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
        nfs::path _p1(p1.nativePath);
        nfs::path _p2(p2.nativePath);
        try {
#ifdef _WITH_BOOST_FILESYSTEM_
            bRes = nfs::copy_file(_p1, _p2, nfs::copy_option::overwrite_if_exists);
#else
            bRes = nfs::copy_file(_p1, _p2, nfs::copy_options::overwrite_existing);
#endif
        } catch (const nfs::filesystem_error& e) {
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
        nfs::path _from(from.nativePath);
        nfs::path _to(to.nativePath);
        try {
            nfs::copy(_from, _to);
            bRes = true;
        } catch (const nfs::filesystem_error& e) {
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
#ifdef _WITH_BOOST_FILESYSTEM_
        boost::system::error_code error_code;
#else
        std::error_code error_code;
#endif
        Path pp = removeLastSeparator(p);
        bool res = nfs::create_directories(nfs::path(pp.nativePath), error_code);
        errorMessage = error_code.message();
        return res;
    }
    //=============================================================================
    static auto
    create_directories(const Path& p, bool& permissionDenied)
    {
#ifdef _WITH_BOOST_FILESYSTEM_
        boost::system::error_code error_code;
#else
        std::error_code error_code;
#endif
        Path pp = removeLastSeparator(p);
        bool res = nfs::create_directories(nfs::path(pp.nativePath), error_code);
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
            res = nfs::create_directory(nfs::path(pp.nativePath));
        } catch (const nfs::filesystem_error& e) {
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
            res = nfs::create_directory(nfs::path(pp.nativePath));
        } catch (const nfs::filesystem_error& e) {
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
            value = nfs::file_size(nfs::path(p.nativePath));
        } catch (const nfs::filesystem_error& e) {
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
#ifndef _WITH_BOOST_FILESYSTEM_
    template <typename TP>
    static std::time_t
    to_time_t(TP tp)
    {
        using namespace std::chrono;
        auto sctp
            = time_point_cast<system_clock::duration>(tp - TP::clock::now() + system_clock::now());
        return system_clock::to_time_t(sctp);
    }
#endif
    //=============================================================================
    static auto
    last_write_time(const Path& p, std::string& errorMessage)
    {
#ifdef _WITH_BOOST_FILESYSTEM_
        time_t file_time;
        errorMessage.clear();
        if (p.native().empty()) {
            file_time = 0;
            return file_time;
        }
        try {
            file_time = nfs::last_write_time(nfs::path(p.nativePath));
        } catch (const nfs::filesystem_error& e) {
            std::error_code error_code = e.code();
            errorMessage = error_code.message();
        }
        return file_time;
#else
        nfs::file_time_type file_time;
        errorMessage.clear();
        if (p.native().empty()) {
            return (time_t)0;
        }
        try {
            file_time = nfs::last_write_time(nfs::path(p.nativePath));
        } catch (const nfs::filesystem_error& e) {
            std::error_code error_code = e.code();
            errorMessage = error_code.message();
        }
        return (time_t)to_time_t(file_time);
#endif
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
        nfs::path p(nativePath);
        return Path(p.lexically_normal().native());
    }
    //=============================================================================
    auto
    lexically_relative(const Path& p)
    {
        nfs::path _p1(nativePath);
        nfs::path _p2(p.nativePath);
        return Path(_p1.lexically_relative(_p2).native());
    }
    //=============================================================================
    static auto
    current_path()
    {
        try {
            return Path(nfs::current_path().native());
        } catch (const nfs::filesystem_error&) {
        }
        return Path();
    }
    //=============================================================================
    static void
    current_path(Path const& p, std::string& errorMessage)
    {
        try {
            nfs::current_path(nfs::path(p.nativePath));
        } catch (const nfs::filesystem_error& e) {
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
        nfs::path p(nativePath);
        return Path(p.parent_path().native());
    }
    //=============================================================================
    static auto
    temp_directory_path()
    {
        return Path(nfs::temp_directory_path().native());
    }
    //=============================================================================
    static Path
    unique_path()
    {
        nfs::path pwd = nfs::temp_directory_path();
#ifdef _WITH_BOOST_FILESYSTEM_
        pwd /= nfs::unique_path();
#else
        pwd /= getUniqueID();
#endif
        if (nfs::exists(pwd)) {
            return unique_path();
        }
        return Path(pwd.native());
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
