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
#ifdef _MSC_VER
#pragma warning(push)
#pragma warning(disable : 4251)
#endif
//=============================================================================
#include <filesystem>
#include <string>
#include <ctime>
//=============================================================================
#include "nlsCommons_exports.h"
//=============================================================================
namespace nfs = std::filesystem;
//=============================================================================
namespace Nelson::FileSystemWrapper {
//=============================================================================
class NLSCOMMONS_IMPEXP Path
{
private:
#ifdef _MSC_VER
    std::wstring nativePath;
#else
    std::string nativePath;
#endif

public:
    Path() = default;
    Path(const std::string& p);
    Path(const std::wstring& p);
    Path(const Path&) = default;
    Path(Path&&) noexcept = default;
    Path&
    operator=(const Path&)
        = default;
    Path&
    operator=(Path&&) noexcept
        = default;

    Path&
    operator=(const std::string& p);
    Path&
    operator=(const std::wstring& p);

    // operators
    Path
    operator/(const Path& p2) const;
    Path
    operator/(const std::string& p2) const;
    Path
    operator/(const std::wstring& p2) const;

    Path&
    operator/=(const Path& p);
    Path&
    operator/=(const std::string& s);
    Path&
    operator/=(const std::wstring& s);

    Path&
    operator+=(const std::string& s);
    Path&
    operator+=(const std::wstring& s);

    // access
#ifdef _MSC_VER
    std::wstring
    native() const;
#else
    std::string
    native() const;
#endif

    std::string
    string() const;
    std::wstring
    wstring() const;
    std::string
    generic_string() const;
    std::wstring
    generic_wstring() const;
    Path
    generic_path() const;

    // helpers
    bool
    empty() const;
    bool
    is_absolute() const;
    bool
    has_filename() const;
    bool
    has_extension() const;

    Path
    filename() const;
    Path
    parent_path() const;
    Path
    stem() const;
    Path
    extension() const;
    Path
    lexically_normal() const;

    Path
    getFinalPathname() const;

    bool
    has_parent_path() const;

    Path
    replace_extension() const;
    Path
    replace_extension(const Path& p) const;

    Path
    lexically_relative(const Path& p) const;

    // filesystem
    bool
    exists() const;
    static bool
    exists(const Path& p);

    bool
    is_directory() const;
    static bool
    is_directory(const Path& p);
    static bool
    is_directory(const std::wstring& p);
    static bool
    is_directory(const std::wstring& p, bool& permissionDenied);

    bool
    is_regular_file() const;
    static bool
    is_regular_file(const Path& p);

    static Path
    current_path();
    static void
    current_path(const Path& p);

    static Path
    temp_directory_path();
    static Path
    canonical(const Path& p);
    static Path
    canonical(const Path& p, std::string& errorMessage);

    static Path
    absolute(const Path& p);
    static Path
    absolute(const Path& p, std::string& errorMessage);

    static bool
    create_directories(const Path& p, std::string& errorMessage);
    static bool
    create_directories(const Path& p, bool& permissionDenied);
    static bool
    create_directories(const Path& p);
    static bool
    create_directories(const std::wstring& wstr);
    static bool
    create_directory(const Path& p, bool& permissionDenied);
    static bool
    create_directory(const Path& p, std::string& errorMessage);
    static bool
    create_directory(const Path& p);

    static bool
    copy_file(const Path& from, const Path& to);
    static bool
    copy_file(const Path& from, const Path& to, std::string& errorMessage);
    static bool
    copy(const Path& from, const Path& to);
    static bool
    copy(const Path& from, const Path& to, std::string& errorMessage);

    static bool
    remove(const Path& p);
    static bool
    remove(const Path& p, std::string& errorMessage);
    static bool
    remove(const std::string& p);
    static bool
    remove(const std::wstring& p);

    static bool
    remove_all(const Path& p);
    static bool
    remove_all(const Path& p, std::string& errorMessage);

    static uintmax_t
    file_size(const Path& p);
    static uintmax_t
    file_size(const Path& p, std::string& errorMessage);
    static bool
    equivalent(const Path& p1, const Path& p2);

    static Path
    unique_path();

    static std::wstring
    getFinalPathname(const std::wstring& path);

    Path
    getFinalPathname();

    static Path
    removeLastSeparator(const Path& p);

    static std::string
    normalize(const std::string& path);
    static std::wstring
    normalize(const std::wstring& path);
    static std::wstring
    normalizeDriveLetter(const std::wstring& path, bool& isDrive);

    static std::time_t
    last_write_time(const Path& p, std::string& errorMessage);

    static bool
    updateFilePermissionsToWrite(const Path& p);
    static bool
    updateFilePermissionsToWrite(const std::wstring& p);

    static bool
    is_directory(const Path& p, bool& permissionDenied);
    static bool
    is_regular_file(const Path& p, bool& permissionDenied);
    static bool
    is_regular_file(const std::wstring& p, bool& permissionDenied);

    static bool
    current_path(const Path& p, std::string& errorMessage);
    static bool
    current_path(const std::wstring& p, std::string& errorMessage);

    static auto
    getUniqueID();
};
//=============================================================================
}
//=============================================================================
#ifdef _MSC_VER
#pragma warning(pop)
#endif
//=============================================================================
