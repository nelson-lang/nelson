//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include <nlohmann/json.hpp>
#include <filesystem>
#include <map>
#include <set>
#include <string>
#include <vector>
//=============================================================================
namespace Nelson::RuntimeInstaller {
namespace fs = std::filesystem;
using Json = nlohmann::json;
inline constexpr const char* metadata = ".nelson-runtime";
struct File
{
    std::string digest;
    unsigned mode = 0644;
};
//=============================================================================
struct Inventory
{
    std::string architecture, version, engine;
    std::vector<std::string> startup, modules;
    std::map<std::string, File> files;
    std::set<std::string> directories;
    bool gui = false, dynamic = false;
};
//=============================================================================
void
require(bool condition, const std::string& message);
bool
plain(const std::string& value);
void
relative(const std::string& value);
void
safePath(const fs::path& path);
bool
pathExists(const fs::path& path);
void
directory(const fs::path& path, unsigned mode = 0755);
std::string
read(const fs::path& path, size_t limit = 64 * 1024 * 1024);
std::string
digest(const std::string& value);
std::string
fileDigest(const fs::path& path);
bool
matches(const fs::path& path, const File& file);
void
write(const fs::path& path, const std::string& value, unsigned mode = 0644);
void
copyNew(const fs::path& from, const fs::path& to, const File& expected);
Json
parse(const std::string& value);
Inventory
decode(const Json& value);
Inventory
stateInventory(const Json& value, const fs::path& registration);
Json
encode(const Inventory& inventory);
Inventory
merge(const Inventory& old, const Inventory& added);
std::string
startupText(const Inventory& inventory);
std::string
manifestText(const Inventory& inventory);
void
verify(const fs::path& root, const Inventory& inventory);
void
prepare(const fs::path& package);
void
install(const fs::path& package, const fs::path& root, const fs::path& registration);
void
uninstall(const fs::path& root);
void
retireCompletedRemoval(const fs::path& root, const fs::path& registration);
void
checkRegistration(const fs::path& registration, const fs::path& root, const std::string& engine);
std::string
registrationText(const fs::path& root);
struct Lock
{
    explicit Lock(const fs::path& filename);
    ~Lock();
    Lock(const Lock&) = delete;
    Lock&
    operator=(const Lock&)
        = delete;
    int descriptor = -1;
};
//=============================================================================
} // namespace Nelson::RuntimeInstaller
//=============================================================================
