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
#include <boost/filesystem.hpp>
#include <boost/filesystem/path.hpp>
//=============================================================================
#include <string>
//=============================================================================
// #include "characters_encoding.hpp"
//=============================================================================
namespace Nelson::FileSystemWrapper {
//=============================================================================
class Path
{
    //=============================================================================
private:
    boost::filesystem::path _path;
    //=============================================================================
public:
    //=============================================================================
    Path() { }
    Path(Path const& p) : _path(p._path) { }
    //=============================================================================
    Path(const std::wstring& p) { _path = boost::filesystem::path(p); }
    //=============================================================================
    Path(const std::string& p) { _path = boost::filesystem::path(p); }
    //=============================================================================
    Path&
    operator=(Path& p)
    {
        _path = std::move(p._path);
        return *this;
    }
    //=============================================================================
    Path&
    assign(Path const& p)
    {
        _path = p._path;
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
        _path = Path(p)._path;
        return *this;
    }
    //=============================================================================
    Path
    operator=(const std::string& p)
    {
        Path p2;
        p2._path = Path(p)._path;
        return p2;
    }
    //=============================================================================
    Path&
    concat(Path const& p)
    {
        _path += p._path;
        return *this;
    }
    //=============================================================================
    Path&
    concat(const std::wstring& s)
    {
        _path += s;
        return *this;
    }
    //=============================================================================
    Path&
    concat(const std::string& s)
    {
        _path += s;
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
        Path p2;
        p2._path = _path.append(p._path.native());
        return p2;
    }
    //=============================================================================
    Path
    operator/=(const std::wstring& s)
    {
        Path p;
        p._path = _path.append(s);
        return p;
    }
    //=============================================================================
    Path
    operator/=(const std::string& s)
    {
        Path p;
        p._path = _path.append(s);
        return p;
    }
    //=============================================================================
    Path
    operator/(const Path& p2)
    {
        Path p;
        p._path = _path / p2._path;
        return p;
    }
    //=============================================================================
    Path
    operator/(const std::string& p2)
    {
        return Path(_path.string() + "/" + p2);
    }
    //=============================================================================
    Path
    operator/(const std::wstring& p2)
    {
        return Path(_path.wstring() + L"/" + p2);
    }
    //=============================================================================

    auto
    native() const
    {
        return _path.native();
    }
    //=============================================================================
    auto
    has_filename() const
    {
        return _path.has_filename();
    }
    //=============================================================================
    auto
    has_extension() const
    {
        return _path.has_extension();
    }
    //=============================================================================
    auto
    extension() const
    {
        return Path(_path.extension().native());
    }
    //=============================================================================
    auto
    wstring() const
    {
        return _path.wstring();
    }
    //=============================================================================
    auto
    string() const
    {
        return _path.string();
    }
    //=============================================================================
    auto
    generic_wstring() const
    {
        return _path.generic_wstring();
    }
    //=============================================================================
    auto
    generic_string() const
    {
        return _path.generic_string();
    }
    //=============================================================================
    auto
    leaf() const
    {
        return Path(_path.leaf().wstring());
    }
    //=============================================================================
    auto
    generic_path() const
    {
        return Path(_path.generic_path().wstring());
    }
    //=============================================================================
    auto
    filename() const
    {
        return Path(_path.filename().wstring());
    }
    //=============================================================================
    auto
    branch_path() const
    {
        return _path.branch_path();
    }
    //=============================================================================
    auto
    stem() const
    {
        return Path(_path.stem().wstring());
    }
    //=============================================================================
    auto
    replace_extension(Path const& new_extension = Path())
    {
        return Path(_path.replace_extension(new_extension._path).wstring());
    }
    //=============================================================================
    auto
    has_parent_path()
    {
        return _path.has_parent_path();
    }
    //=============================================================================
    auto
    has_branch_path()
    {
        return _path.has_branch_path();
    }
    //=============================================================================
    bool
    exists() const
    {
        return boost::filesystem::exists(_path);
    }
    //=============================================================================
    bool
    is_directory() const
    {
        return boost::filesystem::is_directory(_path);
    }
    //=============================================================================
    auto
    normalize()
    {
        return Path(_path.normalize().wstring());
    }
    //=============================================================================
    static bool
    exists(Path const& p)
    {
        return boost::filesystem::exists(p._path);
    }
    //=============================================================================
    static bool
    is_directory(Path const& p)
    {
        return boost::filesystem::is_directory(p._path);
    }
    //=============================================================================
    static auto
    canonical(Path const& p1, Path const& p2)
    {
        return Path(boost::filesystem::canonical(p1._path, p2._path).wstring());
    }
    //=============================================================================
    static bool
    equivalent(Path const& p1, Path const& p2)
    {
        return boost::filesystem::equivalent(p1._path, p2._path);
    }
    //=============================================================================
    static bool
    remove(Path const& p)
    {
        return boost::filesystem::remove(p._path);
    }
    //=============================================================================
    static bool
    remove_all(Path const& p)
    {
        return boost::filesystem::remove_all(p._path);
    }
    //=============================================================================
    auto
    is_absolute()
    {
        return _path.is_absolute();
    }
    //=============================================================================
    static Path
    absolute(Path const& p)
    {
        return Path(boost::filesystem::absolute(p._path).wstring());
    }
    //=============================================================================
    static auto
    copy_file(Path const& p1, Path const& p2)
    {
        return boost::filesystem::copy_file(
            p1._path, p2._path, boost::filesystem::copy_option::overwrite_if_exists);
    }
    //=============================================================================
    static auto
    copy(Path const& from, Path const& to)
    {
        return boost::filesystem::copy(from._path, to._path);
    }
    //=============================================================================
    static auto
    create_directories(const Path& p)
    {
        return boost::filesystem::create_directories(p._path);
    }
    //=============================================================================
    static auto
    create_directory(const Path& p)
    {
        return boost::filesystem::create_directory(p._path);
    }
    //=============================================================================
    static auto
    file_size(const Path& p)
    {
        return boost::filesystem::file_size(p._path);
    }
    //=============================================================================
    static auto
    last_write_time(const Path& p)
    {
        return boost::filesystem::last_write_time(p._path);
    }
    //=============================================================================
    auto
    lexically_normal()
    {
        return Path(_path.lexically_normal().wstring());
    }
    //=============================================================================
    auto
    lexically_relative(const Path& p)
    {
        return Path(_path.lexically_relative(p._path).wstring());
    }
    //=============================================================================
    static auto
    current_path()
    {
        return Path(boost::filesystem::current_path().wstring());
    }
    //=============================================================================
    static auto
    current_path(Path const& p)
    {
        return boost::filesystem::current_path(p.wstring());
    }
    //=============================================================================
    auto
    parent_path()
    {
        return Path(_path.parent_path().wstring());
    }
    //=============================================================================
    static auto
    temp_directory_path()
    {
        return Path(boost::filesystem::temp_directory_path().wstring());
    }
    //=============================================================================
    static auto
    unique_path()
    {
        return Path(boost::filesystem::unique_path().wstring());
    }
    //=============================================================================
};
//=============================================================================
}
//=============================================================================
