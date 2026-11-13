//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <filesystem>
#include <map>
#include <memory>
#include <mutex>
#include <set>
#include "ZipArchiveSessions.hpp"
#include "ZipHelpers.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson::ZipArchiveSessions {
//=============================================================================
namespace {
    std::mutex sessionsMutex;
    std::map<uint64_t, std::unique_ptr<ZipArchive>> sessions;
    std::set<uint64_t> pendingClose;
    uint64_t nextId = 1;
} // namespace
//=============================================================================
static uint64_t
store(ZipArchive archive);
static std::string
folderEntryName(const std::filesystem::path& folder, const std::string& entryName);
static void
requireNewName(const ZipArchive& archive, const std::string& name);
static std::string
folderPrefix(const std::string& name);
static std::vector<size_t>
matchingEntries(const ZipArchive& archive, const std::string& name);
//=============================================================================
uint64_t
openFile(const std::wstring& filename, const std::string& mode)
{
    ZipArchive::Mode openMode = ZipArchive::Mode::Read;
    if (mode == "w") {
        openMode = ZipArchive::Mode::Overwrite;
    } else if (mode == "a") {
        openMode = ZipArchive::Mode::Append;
    } else if (mode == "x") {
        openMode = ZipArchive::Mode::Create;
    } else if (mode != "r") {
        throw ZipArchiveError(ZipErrorCode::InvalidArgument, "Mode must be 'r', 'w', 'a' or 'x'.");
    }
    return store(ZipArchive::openFile(filename, openMode));
}
//=============================================================================
uint64_t
openMemory(std::vector<uint8_t> bytes)
{
    return store(ZipArchive::openMemory(std::move(bytes)));
}
//=============================================================================
uint64_t
createInMemory()
{
    return store(ZipArchive::createInMemory());
}
//=============================================================================
ZipArchive&
get(uint64_t id)
{
    std::lock_guard<std::mutex> lock(sessionsMutex);
    auto it = sessions.find(id);
    if (it == sessions.end()) {
        throw ZipArchiveError(ZipErrorCode::InvalidState, "ZIP archive is closed.");
    }
    return *it->second;
}
//=============================================================================
std::vector<ZipFileItem>
pathItems(const std::wstring& path, const std::string& entryName, const ZipWriteOptions& options)
{
    namespace fs = std::filesystem;
    std::vector<ZipFileItem> items;
    const fs::path source(path);
    std::error_code ec;
    if (!fs::is_directory(source, ec)) {
        ZipFileItem item;
        item.name = entryName.empty() ? wstring_to_utf8(source.filename().wstring()) : entryName;
        item.filename = path;
        item.options = options;
        ZipArchive::fileMetadata(path, item.options.modified, item.options.externalAttributes);
        items.push_back(std::move(item));
        return items;
    }
    const std::string base = folderEntryName(source, entryName);
    for (const std::wstring& file : expandFolders({ source.generic_wstring() })) {
        const fs::path itemPath(file);
        const std::wstring relative = itemPath.lexically_relative(source).generic_wstring();
        ZipFileItem item;
        item.name = base;
        if (!relative.empty() && relative != L".") {
            item.name += "/" + wstring_to_utf8(relative);
        }
        item.filename = file;
        item.options = options;
        item.isDirectory = fs::is_directory(itemPath, ec);
        ZipArchive::fileMetadata(file, item.options.modified, item.options.externalAttributes);
        items.push_back(std::move(item));
    }
    return items;
}
//=============================================================================
std::vector<std::string>
addPath(uint64_t id, const std::wstring& path, const std::string& entryName,
    const ZipWriteOptions& options)
{
    ZipArchive& archive = get(id);
    std::vector<ZipFileItem> items = pathItems(path, entryName, options);
    std::vector<std::string> added;
    for (const ZipFileItem& item : items) {
        const std::string listed = item.isDirectory ? item.name + "/" : item.name;
        requireNewName(archive, listed);
        added.push_back(listed);
    }
    archive.addFiles(items);
    return added;
}
//=============================================================================
std::vector<uint8_t>
close(uint64_t id)
{
    std::unique_ptr<ZipArchive> archive;
    {
        std::lock_guard<std::mutex> lock(sessionsMutex);
        auto it = sessions.find(id);
        if (it == sessions.end()) {
            return {};
        }
        it->second->requireNoEntryBeingWritten();
        archive = std::move(it->second);
        sessions.erase(it);
        pendingClose.erase(id);
    }
    archive->close();
    return archive->memoryBytes();
}
//=============================================================================
void
closeWhenIdle(uint64_t id)
{
    try {
        close(id);
    } catch (const ZipArchiveError& error) {
        if (error.code() != ZipErrorCode::EntryStillOpen) {
            throw;
        }
        std::lock_guard<std::mutex> lock(sessionsMutex);
        pendingClose.insert(id);
    }
}
//=============================================================================
void
finishEntry(uint64_t id)
{
    get(id).endEntry();
    bool closeNow = false;
    {
        std::lock_guard<std::mutex> lock(sessionsMutex);
        closeNow = pendingClose.erase(id) > 0;
    }
    if (closeNow) {
        close(id);
    }
}
//=============================================================================
void
beginEntry(uint64_t id, const std::string& name, const ZipWriteOptions& options)
{
    ZipArchive& archive = get(id);
    archive.requireNoEntryBeingWritten();
    requireNewName(archive, name);
    archive.beginEntry(name, options);
}
//=============================================================================
void
rewrite(const std::wstring& filename, const std::vector<std::string>& removed,
    const std::vector<std::pair<std::string, std::string>>& renamed)
{
    namespace fs = std::filesystem;
    ZipArchive source = ZipArchive::openFile(filename, ZipArchive::Mode::Read);
    const std::vector<ZipEntryInfo>& entries = source.entries();
    // targets[i]: new name of entry i, empty when the entry is removed.
    std::vector<std::string> targets;
    for (const ZipEntryInfo& entry : entries) {
        targets.push_back(entry.name);
    }
    for (const std::string& name : removed) {
        for (size_t i : matchingEntries(source, name)) {
            targets[i].clear();
        }
    }
    for (const auto& names : renamed) {
        const bool folder
            = names.first.empty() || names.first.back() == '/' || !source.find(names.first);
        const std::string oldPrefix = folderPrefix(names.first);
        const std::string newPrefix = folderPrefix(names.second);
        for (size_t i : matchingEntries(source, names.first)) {
            targets[i]
                = folder ? newPrefix + entries[i].name.substr(oldPrefix.size()) : names.second;
        }
    }
    std::set<std::string> finalNames;
    for (const std::string& name : targets) {
        if (!name.empty() && !finalNames.insert(name).second) {
            throw ZipArchiveError(ZipErrorCode::DuplicateEntry, "Duplicate ZIP entry.", name);
        }
    }
    const fs::path target(filename);
    fs::path temporary = target;
    temporary += L".nelson-rewrite";
    std::error_code ec;
    fs::remove(temporary, ec);
    try {
        ZipArchive destination
            = ZipArchive::openFile(temporary.wstring(), ZipArchive::Mode::Create);
        for (size_t i = 0; i < entries.size(); ++i) {
            if (!targets[i].empty()) {
                destination.copyEntry(source, i, targets[i]);
            }
        }
        destination.close();
        source.close();
        fs::rename(temporary, target);
    } catch (...) {
        fs::remove(temporary, ec);
        throw;
    }
}
//=============================================================================
static uint64_t
store(ZipArchive archive)
{
    std::lock_guard<std::mutex> lock(sessionsMutex);
    const uint64_t id = nextId++;
    sessions[id] = std::make_unique<ZipArchive>(std::move(archive));
    return id;
}
//=============================================================================
// Entry prefix of an added folder: the given name, or the folder's own name.
static std::string
folderEntryName(const std::filesystem::path& folder, const std::string& entryName)
{
    std::string base = entryName;
    if (base.empty()) {
        std::filesystem::path name = folder.filename();
        if (name.empty()) {
            name = folder.parent_path().filename();
        }
        base = wstring_to_utf8(name.wstring());
    }
    while (!base.empty() && base.back() == '/') {
        base.pop_back();
    }
    return base;
}
//=============================================================================
static void
requireNewName(const ZipArchive& archive, const std::string& name)
{
    if (archive.find(name)) {
        throw ZipArchiveError(ZipErrorCode::DuplicateEntry, "Duplicate ZIP entry.", name);
    }
}
//=============================================================================
// Folder form of a name: without trailing slashes, then one "/".
static std::string
folderPrefix(const std::string& name)
{
    std::string prefix = name;
    while (!prefix.empty() && prefix.back() == '/') {
        prefix.pop_back();
    }
    return prefix + "/";
}
//=============================================================================
// The entry with this exact name, or every entry under the folder name; throws when none.
static std::vector<size_t>
matchingEntries(const ZipArchive& archive, const std::string& name)
{
    std::vector<size_t> matches;
    if (name.empty()) {
        throw ZipArchiveError(ZipErrorCode::EntryNotFound, "ZIP entry not found.", name);
    }
    if (name.back() != '/') {
        if (auto index = archive.find(name)) {
            matches.push_back(*index);
            return matches;
        }
    }
    const std::string prefix = folderPrefix(name);
    const auto& entries = archive.entries();
    for (size_t i = 0; i < entries.size(); ++i) {
        if (entries[i].name.compare(0, prefix.size(), prefix) == 0) {
            matches.push_back(i);
        }
    }
    if (matches.empty()) {
        throw ZipArchiveError(ZipErrorCode::EntryNotFound, "ZIP entry not found.", name);
    }
    return matches;
}
//=============================================================================
} // namespace Nelson::ZipArchiveSessions
//=============================================================================
