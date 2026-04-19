//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <fmt/format.h>
#include <fmt/xchar.h>
#ifdef _MSC_VER
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <process.h>
#undef min
#undef max
#else
#include <unistd.h>
#endif
#include "FileSystemWrapper.hpp"
#include "characters_encoding.hpp"
#include <chrono>
#include <system_error>
#include "UuidHelpers.hpp"
//=============================================================================
namespace Nelson::FileSystemWrapper {
//=============================================================================
Path::Path(const std::string& p)
{
#ifdef _MSC_VER
    nativePath = utf8_to_wstring(p);
#else
    nativePath = p;
#endif
}
//=============================================================================
Path::Path(const std::wstring& p)
{
#ifdef _MSC_VER
    nativePath = p;
#else
    nativePath = wstring_to_utf8(p);
#endif
}
//=============================================================================
Path&
Path::operator=(const std::string& p)
{
#ifdef _MSC_VER
    nativePath = utf8_to_wstring(p);
#else
    nativePath = p;
#endif
    return *this;
}
//=============================================================================
Path&
Path::operator=(const std::wstring& p)
{
#ifdef _MSC_VER
    nativePath = p;
#else
    nativePath = wstring_to_utf8(p);
#endif
    return *this;
}
//=============================================================================
Path
Path::operator/(const Path& p2) const
{
    return Path((nfs::path(nativePath) / nfs::path(p2.nativePath)).native());
}
//=============================================================================
Path&
Path::operator/=(const Path& p)
{
    nativePath = (nfs::path(nativePath) / nfs::path(p.nativePath)).native();
    return *this;
}
//=============================================================================
Path
Path::operator/(const std::string& s) const
{
    return *this / Path(s);
}
//=============================================================================
Path
Path::operator/(const std::wstring& s) const
{
    return *this / Path(s);
}
//=============================================================================
Path&
Path::operator/=(const std::string& s)
{
    return (*this /= Path(s));
}
//=============================================================================
Path&
Path::operator/=(const std::wstring& s)
{
    return (*this /= Path(s));
}
//=============================================================================
Path&
Path::operator+=(const std::string& s)
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
Path::operator+=(const std::wstring& s)
{
#ifdef _MSC_VER
    nativePath += s;
#else
    nativePath += wstring_to_utf8(s);
#endif
    return *this;
}
//=============================================================================
#ifdef _MSC_VER
std::wstring
Path::native() const
{
    return nativePath;
}
#else
std::string
Path::native() const
{
    return nativePath;
}
#endif
//=============================================================================
std::string
Path::string() const
{
#ifdef _MSC_VER
    return wstring_to_utf8(nativePath);
#else
    return nativePath;
#endif
}
//=============================================================================
std::wstring
Path::wstring() const
{
#ifdef _MSC_VER
    return nativePath;
#else
    return utf8_to_wstring(nativePath);
#endif
}
//=============================================================================
std::string
Path::generic_string() const
{
#ifdef _MSC_VER
    return wstring_to_utf8(nfs::path(nativePath).generic_wstring());
#else
    return nfs::path(nativePath).generic_string();
#endif
}
//=============================================================================
std::wstring
Path::generic_wstring() const
{
#ifdef _MSC_VER
    return nfs::path(nativePath).generic_wstring();
#else
    return utf8_to_wstring(nfs::path(nativePath).generic_string());
#endif
}
//=============================================================================
Path
Path::generic_path() const
{
#ifdef _MSC_VER
    return Path(nfs::path(nativePath).generic_wstring());
#else
    return Path(nfs::path(nativePath).generic_string());
#endif
}
//=============================================================================
bool
Path::empty() const
{
    return nativePath.empty();
}
//=============================================================================
bool
Path::is_absolute() const
{
    return nfs::path(nativePath).is_absolute();
}
//=============================================================================
bool
Path::has_filename() const
{
    return nfs::path(nativePath).has_filename();
}
//=============================================================================
bool
Path::has_extension() const
{
    return nfs::path(nativePath).has_extension();
}
//=============================================================================
Path
Path::filename() const
{
    return Path(nfs::path(nativePath).filename().native());
}
//=============================================================================
Path
Path::parent_path() const
{
    return Path(nfs::path(nativePath).parent_path().native());
}
//=============================================================================
Path
Path::stem() const
{
    return Path(nfs::path(nativePath).stem().native());
}
//=============================================================================
Path
Path::extension() const
{
    return Path(nfs::path(nativePath).extension().native());
}
//=============================================================================
Path
Path::lexically_normal() const
{
    return Path(nfs::path(nativePath).lexically_normal().native());
}
//=============================================================================
bool
Path::exists() const
{
    return nfs::exists(nfs::path(nativePath));
}
//=============================================================================
bool
Path::exists(const Path& p)
{
    return nfs::exists(nfs::path(p.nativePath));
}
//=============================================================================
bool
Path::is_directory() const
{
    return nfs::is_directory(nfs::path(nativePath));
}
//=============================================================================
bool
Path::is_directory(const Path& p)
{
    return nfs::is_directory(nfs::path(p.nativePath));
}
//=============================================================================
bool
Path::is_directory(const std::wstring& p)
{
    return is_directory(Path(p));
}
//=============================================================================
bool
Path::is_directory(const std::wstring& p, bool& permissionDenied)
{
    permissionDenied = false;
    try {
        return is_directory(Path(p));
    } catch (...) {
        permissionDenied = true;
        return false;
    }
}
//=============================================================================
bool
Path::is_regular_file() const
{
    return nfs::is_regular_file(nfs::path(nativePath));
}
//=============================================================================
bool
Path::is_regular_file(const Path& p)
{
    return nfs::is_regular_file(nfs::path(p.nativePath));
}
//=============================================================================
Path
Path::current_path()
{
    return Path(nfs::current_path().native());
}
//=============================================================================
void
Path::current_path(const Path& p)
{
    nfs::current_path(nfs::path(p.nativePath));
}
//=============================================================================
Path
Path::temp_directory_path()
{
    return Path(nfs::temp_directory_path().native());
}
//=============================================================================
Path
Path::canonical(const Path& p)
{
    std::string err;
    return canonical(p, err);
}
//=============================================================================
Path
Path::canonical(const Path& p, std::string& errorMessage)
{
    try {
        errorMessage.clear();
        return Path(nfs::canonical(nfs::path(p.nativePath)).native());
    } catch (const nfs::filesystem_error& e) {
        errorMessage = e.code().message();
        return {};
    }
}
//=============================================================================
Path
Path::absolute(const Path& p)
{
    return Path(nfs::absolute(nfs::path(p.nativePath)).native());
}
//=============================================================================
bool
Path::copy_file(const Path& from, const Path& to)
{
    return nfs::copy_file(nfs::path(from.nativePath), nfs::path(to.nativePath),
        nfs::copy_options::overwrite_existing);
}
//=============================================================================
bool
Path::copy(const Path& from, const Path& to)
{
    nfs::copy(nfs::path(from.nativePath), nfs::path(to.nativePath));
    return true;
}
//=============================================================================
bool
Path::remove(Path const& p, std::string& errorMessage)
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
bool
Path::remove(const Path& p)
{
    std::string message;
    return remove(p, message);
}
//=============================================================================
bool
Path::remove(const std::string& p)
{
    std::string message;
    return remove(Path(p), message);
}
//=============================================================================
bool
Path::remove(const std::wstring& p)
{
    std::string message;
    return remove(Path(p), message);
}
//=============================================================================
bool
Path::remove_all(const Path& p)
{
    return nfs::remove_all(nfs::path(p.nativePath)) > 0;
}
//=============================================================================
uintmax_t
Path::file_size(const Path& p)
{
    return nfs::file_size(nfs::path(p.nativePath));
}
//=============================================================================
bool
Path::equivalent(const Path& p1, const Path& p2)
{
    return nfs::equivalent(nfs::path(p1.nativePath), nfs::path(p2.nativePath));
}
//=============================================================================
auto
Path::getUniqueID()
{
#define TMP_NELSON "{:06x}"
    std::string uuid;
    UuidHelpers::generateUuid(uuid);

#ifdef _MSC_VER
    std::string result = fmt::format(TMP_NELSON, _getpid()) + "-" + uuid + ".tmp";
    return utf8_to_wstring(result);
#else
    std::string result = fmt::format(TMP_NELSON, getpid()) + "-" + uuid + ".tmp";
    return result;
#endif
}
//=============================================================================
Path
Path::unique_path()
{
    nfs::path pwd = nfs::temp_directory_path();
    pwd /= getUniqueID();
    if (nfs::exists(pwd)) {
        return unique_path();
    }
    return Path(pwd.native());
}
//=============================================================================
std::string
Path::normalize(const std::string& path)
{
    return wstring_to_utf8(normalize(utf8_to_wstring(path)));
}
//=============================================================================
std::wstring
Path::normalizeDriveLetter(const std::wstring& path, bool& isDrive)
{
    std::wstring wresult = path;
    if (wresult.length() == std::wstring(L"c:").length() && wresult[1] == L':') {
        wresult[0] = ::towupper(wresult[0]);
        wresult += L"/";
        isDrive = true;
        return wresult;
    }
    if (wresult.length() == std::wstring(L"c:/").length() && wresult[1] == L':'
        && (wresult[2] == L'\\' || wresult[2] == L'/')) {
        wresult[0] = ::towupper(wresult[0]);
        if (wresult[2] == L'\\') {
            wresult[2] = L'/';
        }
        isDrive = true;
        return wresult;
    }
    isDrive = false;
    return wresult;
}
//=============================================================================
std::wstring
Path::getFinalPathname(const std::wstring& path)
{
#ifdef _MSC_VER
#define BUFSIZE MAX_PATH
    HANDLE hFile = CreateFile(path.c_str(), GENERIC_READ, FILE_SHARE_READ | FILE_SHARE_WRITE,
        nullptr, OPEN_EXISTING, FILE_FLAG_BACKUP_SEMANTICS, nullptr);

    if (hFile == INVALID_HANDLE_VALUE) {
        return path;
    }
    wchar_t Path[BUFSIZE];
    DWORD dwRet = GetFinalPathNameByHandle(hFile, Path, BUFSIZE, VOLUME_NAME_DOS);
    CloseHandle(hFile);
    if (dwRet > 4) {
        return std::wstring(Path).substr(4);
    }
#endif
    return path;
}
//=============================================================================
Path
Path::getFinalPathname()
{
#ifdef _MSC_VER
    return Path(getFinalPathname(nativePath));
#endif
    return Path(nativePath);
}
//=============================================================================
std::wstring
Path::normalize(const std::wstring& path)
{
    if (path.empty()) {
        return {};
    }
    bool isDriveLetter;
    std::wstring wresult = normalizeDriveLetter(path, isDriveLetter);
    if (isDriveLetter) {
        return wresult;
    }

    FileSystemWrapper::Path _path(path);
    if (!_path.is_absolute()) {
        _path = FileSystemWrapper::Path::current_path() / _path;
    }
    _path = _path.lexically_normal();

    nfs::path norm_path = nfs::path(_path.native());
    nfs::path prePath;
    nfs::path::iterator it = norm_path.begin();

    for (; it != norm_path.end(); ++it) {
        if (nfs::exists(prePath / *it)) {
            prePath /= *it;
        } else {
            break;
        }
    }
#ifdef _MSC_VER
    std::wstring uniformizedPath = prePath.wstring();
#else
    std::wstring uniformizedPath = utf8_to_wstring(prePath.native());
#endif
    uniformizedPath = FileSystemWrapper::Path::getFinalPathname(uniformizedPath);

#ifdef _MSC_VER
    nfs::path postPath(uniformizedPath);
#else
    nfs::path postPath(wstring_to_utf8(uniformizedPath));
#endif
    for (; it != norm_path.end(); ++it) {
        postPath /= *it;
    }
#ifdef _MSC_VER
    uniformizedPath = postPath.wstring();
#else
    uniformizedPath = utf8_to_wstring(postPath.native());
#endif
    uniformizedPath = FileSystemWrapper::Path(uniformizedPath).generic_wstring();
    if (uniformizedPath.length() > 1 && uniformizedPath.back() == L'/') {
        uniformizedPath.pop_back();
    }
    if (uniformizedPath.length() == std::wstring(L"c:").length()) {
        uniformizedPath += L"/";
    }
    return uniformizedPath;
}
//=============================================================================
bool
Path::has_parent_path() const
{
    return nfs::path(nativePath).has_parent_path();
}
//=============================================================================
Path
Path::replace_extension() const
{
    nfs::path p(nativePath);
    p.replace_extension();
    return Path(p.native());
}
//=============================================================================
Path
Path::replace_extension(const Path& p2) const
{
    nfs::path p(nativePath);
    nfs::path extPath(p2.native());
    p.replace_extension(extPath);
    return Path(p.native());
}
//=============================================================================
Path
Path::lexically_relative(const Path& p) const
{
    try {
        auto rel = nfs::path(nativePath).lexically_relative(nfs::path(p.native()));
        return Path(rel.native());
    } catch (...) {
        return Path();
    }
}
//=============================================================================
Path
Path::absolute(const Path& p, std::string& errorMessage)
{
    try {
        errorMessage.clear();
        return Path(nfs::absolute(nfs::path(p.nativePath)).native());
    } catch (const nfs::filesystem_error& e) {
        errorMessage = e.code().message();
        return {};
    }
}
//=============================================================================
Path
Path::removeLastSeparator(const Path& p)
{
#ifdef _MSC_VER
    std::wstring nativeString = p.nativePath;
    if (nativeString.length() > 1
        && (nativeString.back() == L'/' || nativeString.back() == L'\\')) {
        nativeString.pop_back();
    }
#else
    std::string nativeString = p.nativePath;
    if (nativeString.length() > 1 && (nativeString.back() == '/' || nativeString.back() == '\\')) {
        nativeString.pop_back();
    }
#endif
    return Path(nativeString);
}
//=============================================================================
bool
Path::create_directories(const Path& p, std::string& errorMessage)
{
    std::error_code error_code;
    Path pp = removeLastSeparator(p);
    bool res = nfs::create_directories(nfs::path(pp.nativePath), error_code);
    errorMessage = error_code.message();
    return res;
}
//=============================================================================
bool
Path::create_directories(const Path& p, bool& permissionDenied)
{
    std::error_code error_code;
    permissionDenied = false;
    Path pp = removeLastSeparator(p);
    bool res = nfs::create_directories(nfs::path(pp.nativePath), error_code);
    if (error_code == std::errc::permission_denied) {
        permissionDenied = true;
    }
    return res;
}
//=============================================================================
bool
Path::create_directories(const Path& p)
{
    bool permissionDenied;
    return create_directories(p, permissionDenied);
}
//=============================================================================
bool
Path::create_directories(const std::wstring& wstr)
{
    bool permissionDenied;
    return create_directories(Path(wstr), permissionDenied);
}
//=============================================================================
bool
Path::create_directory(const Path& p, bool& permissionDenied)
{
    bool res = false;
    permissionDenied = false;
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
bool
Path::create_directory(const Path& p, std::string& errorMessage)
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
bool
Path::create_directory(const Path& p)
{
    bool permissionDenied;
    return create_directory(p, permissionDenied);
}
//=============================================================================

bool
Path::copy_file(const Path& from, const Path& to, std::string& errorMessage)
{
    std::error_code ec;
    bool res = nfs::copy_file(nfs::path(from.nativePath), nfs::path(to.nativePath),
        nfs::copy_options::overwrite_existing, ec);
    if (ec) {
        errorMessage = ec.message();
        return false;
    }
    errorMessage.clear();
    return res;
}
//=============================================================================
bool
Path::copy(const Path& from, const Path& to, std::string& errorMessage)
{
    std::error_code ec;
    nfs::copy(nfs::path(from.nativePath), nfs::path(to.nativePath), ec);
    if (ec) {
        errorMessage = ec.message();
        return false;
    }
    errorMessage.clear();
    return true;
}
//=============================================================================
bool
Path::remove_all(const Path& p, std::string& errorMessage)
{
    std::error_code ec;
    auto count = nfs::remove_all(nfs::path(p.nativePath), ec);
    if (ec) {
        errorMessage = ec.message();
        return false;
    }
    errorMessage.clear();
    return count > 0;
}
//=============================================================================
uintmax_t
Path::file_size(const Path& p, std::string& errorMessage)
{
    std::error_code ec;
    auto s = nfs::file_size(nfs::path(p.nativePath), ec);
    if (ec) {
        errorMessage = ec.message();
        return static_cast<uintmax_t>(0);
    }
    errorMessage.clear();
    return s;
}
//=============================================================================
std::time_t
Path::last_write_time(const Path& p, std::string& errorMessage)
{
    errorMessage.clear();
    if (p.native().empty()) {
        return std::time_t { 0 };
    }

    try {
        nfs::file_time_type ftime = nfs::last_write_time(nfs::path(p.nativePath));

        // Convert file_time_type -> system_clock::time_point -> time_t
        using file_clock = decltype(ftime)::clock;
        using namespace std::chrono;

        system_clock::time_point sctp = system_clock::now()
            + duration_cast<system_clock::duration>(ftime - file_clock::now());

        return system_clock::to_time_t(sctp);
    } catch (const nfs::filesystem_error& e) {
        std::error_code error_code = e.code();
        errorMessage = error_code.message();
        return std::time_t { 0 };
    }
} //=============================================================================
bool
Path::updateFilePermissionsToWrite(const Path& p)
{
    std::error_code ec;
    nfs::permissions(nfs::path(p.nativePath),
        nfs::perms::owner_write | nfs::perms::group_write
#ifdef _MSC_VER
            | nfs::perms::others_write
#endif
        ,
        nfs::perm_options::add, ec);
    return !static_cast<bool>(ec);
}
//=============================================================================
bool
Path::updateFilePermissionsToWrite(const std::wstring& p)
{
    return updateFilePermissionsToWrite(Path(p));
}
//=============================================================================
bool
Path::is_directory(const Path& p, bool& permissionDenied)
{
    nfs::path p1(p.native());
    permissionDenied = false;
    try {
        return nfs::exists(p1) && nfs::is_directory(p1);
    } catch (const nfs::filesystem_error& e) {
        if (e.code() == std::errc::permission_denied) {
            permissionDenied = true;
        }
    }
    return false;
}
//=============================================================================
bool
Path::is_regular_file(const Path& p, bool& permissionDenied)
{
    bool bIsFile;
    permissionDenied = false;
    try {
        nfs::path p1(p.nativePath);
        bIsFile = nfs::is_regular_file(p1);
    } catch (const nfs::filesystem_error& e) {
        if (e.code() == std::errc::permission_denied) {
            permissionDenied = true;
        }
        bIsFile = false;
    }
    return bIsFile;
}
//=============================================================================
bool
Path::is_regular_file(const std::wstring& p, bool& permissionDenied)
{
    return is_regular_file(Path(p), permissionDenied);
}
//=============================================================================
bool
Path::current_path(const Path& p, std::string& errorMessage)
{
    std::error_code ec;
    nfs::current_path(nfs::path(p.nativePath), ec);
    if (ec) {
        errorMessage = ec.message();
        return false;
    }
    errorMessage.clear();
    return true;
}
//=============================================================================
bool
Path::current_path(const std::wstring& p, std::string& errorMessage)
{
    return current_path(Path(p), errorMessage);
}
//=============================================================================
}
//=============================================================================
