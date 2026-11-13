//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <map>
#include <memory>
#include <mutex>
#include <set>
#include "TarArchiveSessions.hpp"
#include "ZipArchiveSessions.hpp"
//=============================================================================
namespace Nelson::TarArchiveSessions {
//=============================================================================
namespace {
    struct Session
    {
        std::unique_ptr<TarArchive> archive;
        // Names reserved by file identifiers still being written.
        std::set<std::string> pending;
        bool closeWhenIdle = false;
    };
    std::mutex sessionsMutex;
    std::map<uint64_t, Session> sessions;
    uint64_t nextId = 1;
} // namespace
//=============================================================================
static uint64_t
store(TarArchive archive);
static Session&
session(uint64_t id);
static Session&
lockedSession(uint64_t id);
static void
requireNewName(const Session& entry, const std::string& name);
//=============================================================================
uint64_t
openFile(const std::wstring& filename, const TarReadOptions& options)
{
    return store(TarArchive::openFile(filename, options));
}
//=============================================================================
uint64_t
openMemory(std::vector<uint8_t> bytes, const TarReadOptions& options)
{
    return store(TarArchive::openMemory(std::move(bytes), options));
}
//=============================================================================
uint64_t
create(const std::wstring& filename, TarCompression compression, int level, int threads)
{
    return store(filename.empty()
            ? TarArchive::createInMemory(compression, level, threads)
            : TarArchive::createFile(filename, compression, level, threads));
}
//=============================================================================
TarArchive&
get(uint64_t id)
{
    return *lockedSession(id).archive;
}
//=============================================================================
std::vector<std::string>
addPath(uint64_t id, const std::wstring& path, const std::string& entryName)
{
    Session& entry = lockedSession(id);
    const std::vector<ZipFileItem> items
        = ZipArchiveSessions::pathItems(path, entryName, ZipWriteOptions());
    std::vector<std::string> added;
    for (const ZipFileItem& item : items) {
        const std::string listed = item.isDirectory ? item.name + "/" : item.name;
        requireNewName(entry, listed);
        added.push_back(listed);
    }
    entry.archive->addFiles(items);
    return added;
}
//=============================================================================
void
addBuffer(uint64_t id, const std::string& name, const std::vector<uint8_t>& data, time_t modified)
{
    Session& entry = lockedSession(id);
    requireNewName(entry, name);
    entry.archive->addBuffer(name, data.data(), data.size(), modified);
}
//=============================================================================
void
beginEntry(uint64_t id, const std::string& name)
{
    Session& entry = lockedSession(id);
    if (!entry.archive->isWriting()) {
        throw ZipArchiveError(
            ZipErrorCode::InvalidState, "The tar archive is not open for writing.");
    }
    requireNewName(entry, name);
    entry.pending.insert(name);
}
//=============================================================================
void
finishEntry(uint64_t id, const std::string& name, const std::wstring& spooledFile)
{
    bool closeNow = false;
    {
        Session& entry = lockedSession(id);
        entry.pending.erase(name);
        entry.archive->addFile(name, spooledFile);
        closeNow = entry.closeWhenIdle && entry.pending.empty();
    }
    if (closeNow) {
        close(id);
    }
}
//=============================================================================
std::vector<uint8_t>
close(uint64_t id)
{
    std::unique_ptr<TarArchive> archive;
    {
        std::lock_guard<std::mutex> lock(sessionsMutex);
        auto it = sessions.find(id);
        if (it == sessions.end()) {
            return { };
        }
        if (!it->second.pending.empty()) {
            throw ZipArchiveError(ZipErrorCode::EntryStillOpen,
                "A tar entry is still written through a file identifier.",
                *it->second.pending.begin());
        }
        archive = std::move(it->second.archive);
        sessions.erase(it);
    }
    archive->close();
    return archive->memoryBytes();
}
//=============================================================================
void
closeWhenIdle(uint64_t id)
{
    {
        std::lock_guard<std::mutex> lock(sessionsMutex);
        auto it = sessions.find(id);
        if (it == sessions.end()) {
            return;
        }
        if (!it->second.pending.empty()) {
            it->second.closeWhenIdle = true;
            return;
        }
    }
    close(id);
}
//=============================================================================
static uint64_t
store(TarArchive archive)
{
    std::lock_guard<std::mutex> lock(sessionsMutex);
    const uint64_t id = nextId++;
    sessions[id].archive = std::make_unique<TarArchive>(std::move(archive));
    return id;
}
//=============================================================================
static Session&
session(uint64_t id)
{
    auto it = sessions.find(id);
    if (it == sessions.end()) {
        throw ZipArchiveError(ZipErrorCode::InvalidState, "tar archive is closed.");
    }
    return it->second;
}
//=============================================================================
static Session&
lockedSession(uint64_t id)
{
    std::lock_guard<std::mutex> lock(sessionsMutex);
    return session(id);
}
//=============================================================================
static void
requireNewName(const Session& entry, const std::string& name)
{
    if (entry.archive->find(name) || entry.pending.count(name) != 0) {
        throw ZipArchiveError(ZipErrorCode::DuplicateEntry, "Duplicate tar entry.", name);
    }
}
//=============================================================================
} // namespace Nelson::TarArchiveSessions
//=============================================================================
