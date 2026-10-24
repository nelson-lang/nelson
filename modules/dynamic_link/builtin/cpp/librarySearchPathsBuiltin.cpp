//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
// SPDX-License-Identifier: LGPL-3.0-or-later
//=============================================================================
#include "librarySearchPathsBuiltin.hpp"
#include "Error.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include <algorithm>
#include <filesystem>
#include <mutex>
#include <stdexcept>
#ifdef _WIN32
#include <windows.h>
#endif
namespace Nelson::DynamicLinkGateway {
//=============================================================================
namespace {
    namespace fs = std::filesystem;
    class SearchPaths
    {
    public:
        wstringVector directories;
#ifdef _WIN32
        std::vector<DLL_DIRECTORY_COOKIE> cookies;
#endif
        SearchPaths() = default;
        SearchPaths(const SearchPaths&) = delete;
        SearchPaths&
        operator=(const SearchPaths&)
            = delete;

        ~SearchPaths()
        {
#ifdef _WIN32
            for (auto cookie : cookies) {
                RemoveDllDirectory(cookie);
            }
#endif
        }

        void
        prepare(const wstringVector& input)
        {
            if (input.size() > 256) {
                throw std::runtime_error("Native search directory count limit exceeded.");
            }
            for (const auto& value : input) {
                const fs::path path(value);
                if (value.find(L'\0') != std::wstring::npos || !path.is_absolute()
                    || !fs::is_directory(path)) {
                    throw std::runtime_error(
                        "Native search directories must exist and be absolute.");
                }
                const auto canonical = fs::canonical(path);
                const auto duplicate = std::any_of(
                    directories.begin(), directories.end(), [&](const std::wstring& previous) {
                        return fs::equivalent(previous, canonical);
                    });
                if (!duplicate) {
                    directories.push_back(canonical.wstring());
                }
            }
#ifdef _WIN32
            cookies.reserve(directories.size());
            for (const auto& directory : directories) {
                const auto cookie = AddDllDirectory(directory.c_str());
                if (!cookie) {
                    throw std::runtime_error("Cannot register native search directory (Windows "
                        + std::to_string(GetLastError()) + ").");
                }
                cookies.push_back(cookie);
            }
#else
            if (!directories.empty()) {
                throw std::runtime_error("Native application search directories require Windows.");
            }
#endif
        }

        void
        swap(SearchPaths& other) noexcept
        {
            directories.swap(other.directories);
#ifdef _WIN32
            cookies.swap(other.cookies);
#endif
        }
    };
    //=============================================================================
} // namespace
//=============================================================================
ArrayOfVector
librarySearchPathsBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 0, 1);
    nargoutcheck(nLhs, 0, 1);
    static std::mutex mutex;
    static SearchPaths current;
    try {
        std::lock_guard<std::mutex> lock(mutex);
        ArrayOfVector result { ArrayOf::cellArrayFromArrayOfCharacterRowVectors(
            current.directories) };
        if (!argIn.empty()) {
            SearchPaths replacement;
            replacement.prepare(argIn[0].asWideStringVector());
            // Keep the old registration intact until every new directory is registered.
            current.swap(replacement);
        }
        return result;
    } catch (const std::exception& error) {
        RaiseError(error.what(), "Nelson:nelsonc:nativeSearchPath");
    }
    return {};
}
//=============================================================================
} // namespace Nelson::DynamicLinkGateway
//=============================================================================
