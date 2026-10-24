//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
// SPDX-License-Identifier: LGPL-3.0-or-later
//=============================================================================
#include "ApplicationInputs.hpp"
#include "ApplicationDependencies.hpp"
#include "characters_encoding.hpp"
#include "nlohmann/json.hpp"
#include <algorithm>
#include <cwctype>
#include <map>
#include <set>
#ifdef _WIN32
#include <windows.h>
#endif
namespace Nelson::Application {
namespace fs = std::filesystem;
//=============================================================================
namespace {
    bool
    specialDirectory(const fs::path& directory)
    {
        const auto name = directory.filename().u8string();
        return name == "private" || (!name.empty() && (name[0] == '+' || name[0] == '@'));
    }
    //=============================================================================
    void
    requireOrdinaryInput(const fs::path& path)
    {
        bool linked = fs::is_symlink(fs::symlink_status(path));
#ifdef _WIN32
        const auto attributes = GetFileAttributesW(path.c_str());
        linked = linked
            || (attributes != INVALID_FILE_ATTRIBUTES
                && (attributes & FILE_ATTRIBUTE_DIRECTORY) != 0
                && (attributes & FILE_ATTRIBUTE_REPARSE_POINT) != 0);
#endif
        if (linked) {
            throw std::runtime_error("Explicit application inputs cannot contain symbolic links or "
                                     "directory reparse points: "
                + path.u8string());
        }
        if (!fs::is_regular_file(path) && !fs::is_directory(path)) {
            throw std::runtime_error(
                "Explicit application input must be a regular file or directory: "
                + path.u8string());
        }
    }
    //=============================================================================
    bool
    filenameMatches(const std::wstring& pattern, const std::wstring& name)
    {
        // Dynamic programming bounds work even for repeated wildcard/literal pairs.
        std::vector<bool> matches(name.size() + 1, false);
        matches[0] = true;
        for (wchar_t token : pattern) {
            bool diagonal = matches[0];
            matches[0] = matches[0] && token == L'*';
            for (size_t k = 1; k <= name.size(); ++k) {
                const bool previous = matches[k];
                wchar_t letter = name[k - 1];
#ifdef _WIN32
                token = static_cast<wchar_t>(std::towlower(token));
                letter = static_cast<wchar_t>(std::towlower(letter));
#endif
                matches[k] = token == L'*' ? matches[k] || matches[k - 1]
                                           : diagonal && (token == L'?' || token == letter);
                diagonal = previous;
            }
        }
        return matches.back();
    }
    //=============================================================================
    class InputCollector
    {
    public:
        void
        add(const fs::path& requested)
        {
            const auto path = fs::absolute(requested).lexically_normal();
            std::error_code error;
            if (fs::exists(path, error) || fs::is_symlink(fs::symlink_status(path, error))) {
                requireOrdinaryInput(path);
                if (fs::is_directory(path)) {
                    directory(path);
                } else {
                    retain(path, false);
                    lookup(functionSearchRoot(fs::canonical(path).parent_path()));
                }
                return;
            }
            if (path.filename().u8string().find_first_of("*?") == std::string::npos) {
                throw std::runtime_error(
                    "Explicit application input does not exist: " + path.u8string());
            }
            const auto parent = path.parent_path();
            require(fs::is_directory(parent),
                "Application filename pattern requires an existing parent directory.");
            requireOrdinaryInput(parent);
            const auto pattern = utf8_to_wstring(path.filename().u8string());
            require(pattern.size() <= 1024, "Application filename pattern is too long.");
            std::vector<fs::path> matches;
            for (const auto& item : fs::directory_iterator(parent)) {
                if (filenameMatches(pattern, utf8_to_wstring(item.path().filename().u8string()))
                    && !item.is_directory()) {
                    requireOrdinaryInput(item.path());
                    matches.push_back(item.path());
                    require(matches.size() <= 10000,
                        "Explicit application input count limit exceeded.");
                }
            }
            if (matches.empty()) {
                throw std::runtime_error(
                    "Application filename pattern matched no files: " + path.u8string());
            }
            std::sort(matches.begin(), matches.end());
            const auto root = functionSearchRoot(fs::canonical(parent));
            anchor(root);
            lookup(root);
            for (const auto& file : matches) {
                retain(file, false);
            }
        }

        InputSelection
        finish()
        {
            for (const auto& item : files) {
                result.files.push_back(item.second);
            }
            for (const auto& item : directories) {
                result.directories.push_back(item.second);
            }
            return result;
        }

    private:
        void
        retain(const fs::path& path, bool isDirectory)
        {
            const auto canonical = fs::canonical(path);
            auto& collection = isDirectory ? directories : files;
            collection.emplace(fileKey(canonical), canonical);
            require(files.size() + directories.size() <= 10000,
                "Explicit application input count limit exceeded.");
        }

        void
        anchor(const fs::path& path)
        {
            if (rootKeys.insert(fileKey(path)).second) {
                result.roots.push_back(path);
            }
        }

        void
        lookup(const fs::path& path)
        {
            if (lookupKeys.insert(fileKey(path)).second) {
                result.lookupPaths.push_back(path);
            }
        }

        void
        directory(const fs::path& source)
        {
            const auto path = fs::canonical(source);
            const auto root = functionSearchRoot(path);
            anchor(root);
            retain(path, true);
            lookup(root);
            std::vector<fs::path> children;
            for (fs::recursive_directory_iterator it(path), end; it != end; ++it) {
                requireOrdinaryInput(it->path());
                retain(it->path(), it->is_directory());
                if (it->is_directory()) {
                    children.push_back(it->path());
                }
            }
            std::sort(children.begin(), children.end());
            for (const auto& child : children) {
                const auto relative = child.lexically_relative(root);
                if (std::none_of(relative.begin(), relative.end(), specialDirectory)) {
                    lookup(child);
                }
            }
        }

        InputSelection result;
        std::map<std::string, fs::path> files;
        std::map<std::string, fs::path> directories;
        std::set<std::string> rootKeys;
        std::set<std::string> lookupKeys;
    };
    //=============================================================================
} // namespace
//=============================================================================
std::string
fileKey(const fs::path& path)
{
    auto key = fs::weakly_canonical(path).generic_u8string();
#ifdef _WIN32
    auto wide = utf8_to_wstring(key);
    std::transform(wide.begin(), wide.end(), wide.begin(),
        [](wchar_t ch) { return static_cast<wchar_t>(std::towlower(ch)); });
    key = wstring_to_utf8(wide);
#endif
    return key;
}
//=============================================================================
bool
within(const fs::path& path, const fs::path& root)
{
    const auto relative = path.lexically_relative(root);
    return !relative.empty() && *relative.begin() != ".." && !relative.is_absolute();
}
//=============================================================================
fs::path
functionSearchRoot(fs::path directory)
{
    while (!directory.empty() && specialDirectory(directory)) {
        directory = directory.parent_path();
    }
    return directory;
}
//=============================================================================
InputSelection
expandInputs(const std::vector<std::wstring>& inputs)
{
    InputCollector collector;
    for (const auto& input : inputs) {
        require(!input.empty(), "Explicit application input must not be empty.");
        collector.add(fs::u8path(wstring_to_utf8(input)));
    }
    return collector.finish();
}
//=============================================================================
PackageSelection
readPackageSelection(const std::wstring& encoded)
{
    PackageSelection result;
    if (encoded.empty()) {
        return result;
    }
    const auto policy = nlohmann::json::parse(wstring_to_utf8(encoded));
    require(policy.is_object() && policy.size() == 3, "Invalid support-package selection policy.");
    for (const auto* field : { "roots", "modules", "lookup" }) {
        require(policy.at(field).is_array() && policy.at(field).size() <= 10000,
            "Invalid or oversized support-package selection list.");
    }
    for (const auto* field : { "roots", "lookup" }) {
        for (const auto& value : policy.at(field)) {
            const auto path = fs::u8path(value.get<std::string>());
            require(path.is_absolute() && fs::is_directory(path),
                "Support packages require existing absolute directories.");
            if (std::string(field) == "roots") {
                result.excludedRoots.push_back(fs::u8path(fileKey(path)));
            } else {
                result.lookupPaths.push_back(fs::canonical(path));
            }
        }
    }
    for (const auto& module : policy.at("modules")) {
        const auto name = module.get<std::string>();
        require(!name.empty(), "A support-package name must not be empty.");
        result.excludedModules.insert(name);
    }
    return result;
}
//=============================================================================
} // namespace Nelson::Application
//=============================================================================
