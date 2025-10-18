//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <fmt/printf.h>
#include <fmt/format.h>
#include <fmt/xchar.h>
#include <atomic>
#include <filesystem>
#include <cstdio>
#include <string>
#include <cstring>
#include <cctype>
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
#include "FileSystemWrapper.hpp"
#include "UuidHelpers.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson::FileSystemWrapper {
//=============================================================================
auto
Path::getUniqueID()
{
#ifdef _MSC_VER
    std::wstring uuid;
    UuidHelpers::generateUuid(uuid);
#define TMP_NELSON L"%06x"
    std::wstring result = fmt::sprintf(TMP_NELSON, _getpid()) + L"-" + uuid + L".tmp";
    return result;
#else
#define TMP_NELSON "%06x"
    std::string uuid;
    UuidHelpers::generateUuid(uuid);
    std::string result = fmt::sprintf(TMP_NELSON, (int)getpid()) + "-" + uuid + ".tmp";
    return result;
#endif
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
Path::Path(const std::wstring& p)
{
#ifdef _MSC_VER
    nativePath = p;
#else
    nativePath = wstring_to_utf8(p);
#endif
}
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
Path&
Path::operator=(Path& p)
{
    nativePath = std::move(p.nativePath);
    return *this;
}
//=============================================================================
Path&
Path::assign(Path const& p)
{
    nativePath = p.nativePath;
    return *this;
}
//=============================================================================
Path&
Path::operator=(Path const& p)
{
    return Path::assign(p);
}
//=============================================================================
Path&
Path::operator=(const std::wstring& p)
{
    nativePath = Path(p).nativePath;
    return *this;
}
//=============================================================================
Path
Path::operator=(const std::string& p)
{
    nativePath = Path(p).nativePath;
    return (Path) * this;
}
//=============================================================================
Path&
Path::concat(Path const& p)
{
    nativePath += p.nativePath;
    return *this;
}
//=============================================================================
Path&
Path::concat(const std::wstring& s)
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
Path::concat(const std::string& s)
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
Path::operator+=(const std::string& s)
{
    return concat(s);
}
//=============================================================================
Path&
Path::operator+=(const std::wstring& s)
{
    return concat(s);
}
//=============================================================================
Path
Path::operator/=(Path const& p)
{
    nfs::path pr(nativePath);
    nfs::path pa(p.nativePath);
    pr /= pa;
    nativePath = pr.native();
    return (Path) * this;
}
//=============================================================================
Path
Path::operator/=(const std::wstring& s)
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
Path::operator/=(const std::string& s)
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
Path::operator/(const Path& p2)
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
Path::operator/(const std::string& p2)
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
Path::operator/(const std::wstring& p2)
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
#ifdef _MSC_VER
std::wstring
Path::native() const
#else
std::string
Path::native() const
#endif
{
    return nativePath;
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
Path::extension() const
{
    return Path(nfs::path(nativePath).extension().native());
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
Path::generic_wstring() const
{
#ifdef _MSC_VER
    return nfs::path(nativePath).generic_wstring();
#else
    return utf8_to_wstring(nfs::path(nativePath).generic_string());
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
Path
Path::filename() const
{
#ifdef _MSC_VER
    return Path(nfs::path(nativePath).filename().wstring());
#else
    return Path(nfs::path(nativePath).filename().string());
#endif
}
//=============================================================================
Path
Path::parent_path() const
{
    nfs::path p(nativePath);
    return Path(p.parent_path().native());
}
//=============================================================================
Path
Path::stem() const
{
    nfs::path p(nativePath);
    return Path(p.stem().native());
}
//=============================================================================
Path
Path::replace_extension(Path const& new_extension)
{
    nfs::path p1(nativePath);
    nfs::path ext(new_extension.native());
    return Path(p1.replace_extension(ext).native());
}
//=============================================================================
bool
Path::has_parent_path()
{
    nfs::path p1(nativePath);
    return p1.has_parent_path();
}
//=============================================================================
bool
Path::exists(std::string& errorMessage) const
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
bool
Path::exists() const
{
    std::string errorMessage;
    return exists(errorMessage);
}
//=============================================================================
bool
Path::exists(Path const& p)
{
    return p.exists();
}
//=============================================================================
bool
Path::is_directory(const Path& path, bool& permissionDenied)
{
    nfs::path p1(path.native());
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
Path::is_directory(const std::wstring& path, bool& permissionDenied)
{
    Path p1(path);
    return is_directory(p1, permissionDenied);
}
//=============================================================================
bool
Path::is_directory(const std::wstring& path)
{
    bool permissionDenied;
    return is_directory(path, permissionDenied);
}
//=============================================================================
bool
Path::is_directory(const Path& path)
{
    bool permissionDenied;
    return is_directory(path, permissionDenied);
}
//=============================================================================
bool
Path::is_directory() const
{
    return is_directory(Path(nativePath));
}
//=============================================================================
bool
Path::is_regular_file(const Path& filePath, bool& permissionDenied)
{
    bool bIsFile;
    permissionDenied = false;
    try {
        nfs::path p1(filePath.nativePath);
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
Path::is_regular_file(const Path& filePath)
{
    bool permissionDenied;
    return is_regular_file(filePath, permissionDenied);
}
//=============================================================================
bool
Path::is_regular_file(const std::wstring& filePath, bool& permissionDenied)
{
    Path p1(filePath);
    return is_regular_file(p1, permissionDenied);
}
//=============================================================================
bool
Path::is_regular_file(const std::wstring& filePath)
{
    bool permissionDenied;
    return is_regular_file(filePath, permissionDenied);
}
//=============================================================================
bool
Path::is_regular_file() const
{
    Path p1(nativePath);
    bool permissionDenied;
    return is_regular_file(p1, permissionDenied);
}
//=============================================================================
Path
Path::canonical(Path const& p1, std::string& errorMessage)
{
    nfs::path p(p1.nativePath);
    try {
        return Path(nfs::canonical(p).native());
    } catch (const nfs::filesystem_error& e) {
        std::error_code error_code = e.code();
        errorMessage = error_code.message();
    }
    return {};
}
//=============================================================================
bool
Path::equivalent(Path const& p1, Path const& p2, std::string& errorMessage)
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
bool
Path::equivalent(Path const& p1, Path const& p2)
{
    std::string errorMessage;
    return equivalent(p1, p2, errorMessage);
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
Path::remove(Path const& p)
{
    std::string message;
    return remove(p, message);
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
Path::remove(const std::string& p)
{
    std::string message;
    return remove(Path(p), message);
}
//=============================================================================

bool
Path::remove_all(Path const& p, std::string& errorMessage)
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
bool
Path::remove_all(Path const& p)
{
    std::string message;
    return remove_all(p, message);
}
//=============================================================================
bool
Path::is_absolute()
{
    return nfs::path(nativePath).is_absolute();
}
//=============================================================================
Path
Path::absolute(Path const& p, std::string& errorMessage)
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
Path
Path::absolute(Path const& p)
{
    std::string errorMessage;
    return absolute(p, errorMessage);
}
//=============================================================================
bool
Path::copy_file(Path const& p1, Path const& p2, std::string& errorMessage)
{
    bool bRes = false;
    nfs::path _p1(p1.nativePath);
    nfs::path _p2(p2.nativePath);
    try {
        bRes = nfs::copy_file(_p1, _p2, nfs::copy_options::overwrite_existing);
    } catch (const nfs::filesystem_error& e) {
        std::error_code error_code = e.code();
        errorMessage = error_code.message();
        bRes = false;
    }
    return bRes;
}
//=============================================================================
bool
Path::copy_file(Path const& p1, Path const& p2)
{
    std::string errorMessage;
    return copy_file(p1, p2, errorMessage);
}
//=============================================================================
bool
Path::copy(Path const& from, Path const& to, std::string& errorMessage)
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
bool
Path::copy(Path const& from, Path const& to)
{
    std::string errorMessage;
    return copy(from, to, errorMessage);
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
uintmax_t
Path::file_size(const Path& p, std::string& errorMessage)
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
uintmax_t
Path::file_size(const Path& p)
{
    std::string errorMessage;
    return file_size(p, errorMessage);
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
}
//=============================================================================
std::time_t
Path::last_write_time(const Path& p)
{
    std::string errorMessage;
    return last_write_time(p, errorMessage);
}
//=============================================================================
Path
Path::lexically_normal()
{
    nfs::path p(nativePath);
    return Path(p.lexically_normal().native());
}
//=============================================================================
Path
Path::lexically_relative(const Path& p)
{
    nfs::path _p1(nativePath);
    nfs::path _p2(p.nativePath);
    return Path(_p1.lexically_relative(_p2).native());
}
//=============================================================================
Path
Path::current_path()
{
    try {
        return Path(nfs::current_path().native());
    } catch (const nfs::filesystem_error&) {
    }
    return {};
}
//=============================================================================
void
Path::current_path(Path const& p, std::string& errorMessage)
{
    try {
        nfs::current_path(nfs::path(p.nativePath));
    } catch (const nfs::filesystem_error& e) {
        std::error_code error_code = e.code();
        errorMessage = error_code.message();
    }
}
//=============================================================================
void
Path::current_path(Path const& p)
{
    std::string errorMessage;
    current_path(p, errorMessage);
}
//=============================================================================
void
Path::current_path(const std::wstring& wstr)
{
    std::string errorMessage;
    current_path(Path(wstr), errorMessage);
}
//=============================================================================
Path
Path::parent_path()
{
    nfs::path p(nativePath);
    return Path(p.parent_path().native());
}
//=============================================================================
Path
Path::temp_directory_path()
{
    return Path(nfs::temp_directory_path().native());
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
Path
Path::normalize()
{
    return Path(normalize(nativePath));
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

    for (it; it != norm_path.end(); ++it) {
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
    for (it; it != norm_path.end(); ++it) {
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
Path::updateFilePermissionsToWrite(const Path& filePath)
{
    try {
        nfs::permissions(filePath.native(),
            nfs::perms::owner_write | nfs::perms::group_write | nfs::perms::others_write,
            nfs::perm_options::add);
        if (is_directory(filePath)) {
            for (nfs::recursive_directory_iterator p(filePath.native()), end; p != end; ++p) {
                updateFilePermissionsToWrite(FileSystemWrapper::Path(p->path().native()));
            }
        }
        return true;
    } catch (const nfs::filesystem_error&) {
    }
    return false;
}
//=============================================================================
bool
Path::updateFilePermissionsToWrite(const std::wstring& folderName)
{
    FileSystemWrapper::Path filePath(folderName);
    return updateFilePermissionsToWrite(filePath);
}
//=============================================================================
};
//=============================================================================
