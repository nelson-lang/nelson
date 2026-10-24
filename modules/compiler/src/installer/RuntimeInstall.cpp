//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "RuntimeInstaller.hpp"
#include <iostream>
#include <sys/stat.h>
#include <unistd.h>
//=============================================================================
namespace Nelson::RuntimeInstaller {
//=============================================================================
namespace {
#ifdef __APPLE__
    constexpr const char* architectureDirectory =
#if defined(__aarch64__) || defined(__arm64__)
        "maca64";
#else
        "maci64";
#endif
#else
    constexpr const char* architectureDirectory = "glnxa64";
#endif
    constexpr const char* inventoryName = "runtime-package.json";
    Json
    load(const fs::path& path)
    {
        return parse(read(path));
    }
    //=============================================================================
    Json
    state(const Inventory& inventory, const std::string& helper, const fs::path& registration)
    {
        return { { "format", 1 }, { "inventory", encode(inventory) }, { "helperDigest", helper },
            { "registration", registration.generic_u8string() } };
    }
    //=============================================================================
    void
    checkFile(const fs::path& path, const std::string& oldDigest, const std::string& newDigest,
        unsigned mode, bool recovering)
    {
        if (!pathExists(path)) {
            require(oldDigest.empty(), "Missing installed runtime metadata.");
            return;
        }
        require((!oldDigest.empty() && matches(path, { oldDigest, mode }))
                || (recovering && matches(path, { newDigest, mode })),
            "Runtime metadata changed: " + path.u8string());
    }
    //=============================================================================
    void
    checkMetadata(const fs::path& root, const Json& before, const Json& after, bool recovering)
    {
        const auto added = decode(after.at("inventory"));
        const bool previous = !before.is_null();
        const auto old = previous ? decode(before.at("inventory")) : Inventory {};
        checkFile(root / "modules/modules.m", previous ? digest(startupText(old)) : "",
            digest(startupText(added)), 0644, recovering);
        checkFile(root / "runtime.json", previous ? digest(manifestText(old)) : "",
            digest(manifestText(added)), 0644, recovering);
        checkFile(root / metadata / "uninstall",
            previous ? before.at("helperDigest").get<std::string>() : "",
            after.at("helperDigest").get<std::string>(), 0755, recovering);
        checkFile(root / metadata / "inventory.json", previous ? digest(before.dump() + "\n") : "",
            digest(after.dump() + "\n"), 0644, recovering);
    }
    //=============================================================================
    void
    emptyDestination(const fs::path& root)
    {
        for (const auto& entry : fs::directory_iterator(root)) {
            require(entry.path().filename() == metadata,
                "Runtime destination is not empty or managed.");
        }
        for (const auto& entry : fs::directory_iterator(root / metadata)) {
            require(entry.path().filename() == "operation.lock",
                "Unrecognized runtime preparation data.");
        }
    }
    //=============================================================================
    void
    checkAdditions(
        const fs::path& root, const Inventory& added, const Inventory* previous, bool recovering)
    {
        for (const auto& name : added.directories) {
            const auto path = root / fs::u8path(name);
            safePath(path);
            require(!pathExists(path) || fs::is_directory(path),
                "Runtime directory collides with another file.");
        }
        for (const auto& [name, file] : added.files) {
            const auto path = root / fs::u8path(name);
            safePath(path);
            if (previous && previous->files.count(name)) {
                continue;
            }
            require(!pathExists(path) || (recovering && matches(path, file)),
                "Unmanaged or changed runtime addition: " + name);
        }
    }
    //=============================================================================
} // namespace
//=============================================================================
void
prepare(const fs::path& package)
{
    auto value = load(package / inventoryName);
    auto& files = value.at("files");
    if (files.is_object()) {
        files = Json::array({ files });
    }
    require(files.is_array() && files.size() <= 100000, "Invalid package file inventory.");
    for (auto& item : files) {
        const auto name = item.at("relative").get<std::string>();
        relative(name);
        const auto path = package / "payload" / fs::u8path(name);
        safePath(path);
        struct stat info {};
        require(
            ::lstat(path.c_str(), &info) == 0 && S_ISREG(info.st_mode), "Missing package payload.");
        const unsigned mode = (info.st_mode & 0111) ? 0755 : 0644;
        require(::chmod(path.c_str(), mode) == 0, "Cannot normalize package file permissions.");
        item["mode"] = mode;
    }
    const auto inventory = decode(value);
    verify(package / "payload", inventory);
    // Normalize the helper permissions first: a build tree with a umask that
    // leaves the binary group-writable (0775) must still package it as 0755.
    const auto helperPath = package / "runtime-installer";
    struct stat helperInfo {};
    if (::lstat(helperPath.c_str(), &helperInfo) == 0 && S_ISREG(helperInfo.st_mode)) {
        require(::chmod(helperPath.c_str(), 0755) == 0,
            "Cannot normalize the runtime installer helper permissions.");
    }
    require(matches(helperPath, { fileDigest(helperPath), 0755 }),
        "Missing executable runtime installer helper.");
    write(package / inventoryName, encode(inventory).dump() + "\n");
}
//=============================================================================
std::string
registrationText(const fs::path& root)
{
    return "NELSON_RUNTIME_ROOT_1\n" + root.generic_u8string() + "\n";
}
//=============================================================================
void
checkRegistration(const fs::path& registration, const fs::path& root, const std::string& engine)
{
    safePath(registration);
    require(registration.filename() == engine + ".root"
            && registration.parent_path().filename() == architectureDirectory
            && registration.parent_path().parent_path().filename() == "runtimes"
            && registration.parent_path().parent_path().parent_path().filename() == "nelson",
        "Invalid runtime registration path.");
    require(!pathExists(registration) || read(registration, 32768) == registrationText(root),
        "This runtime is already registered at another location or its registration changed.");
}
//=============================================================================
void
install(const fs::path& package, const fs::path& root, const fs::path& registration)
{
    safePath(package);
    safePath(root);
    safePath(registration);
    require(root != root.root_path(), "Cannot install into the filesystem root.");
    require(root.native().find_first_of(":;$") == std::string::npos,
        "Unsupported runtime library path.");
    const auto added = decode(load(package / inventoryName));
    const auto helper = fileDigest(package / "runtime-installer");
    verify(package / "payload", added);
    directory(registration.parent_path(), ::geteuid() == 0 ? 0755 : 0700);
    Lock registered(registration.string() + ".lock");
    checkRegistration(registration, root, added.engine);
    directory(root);
    directory(root / metadata, 0700);
    Lock locked(root / metadata / "operation.lock");
    retireCompletedRemoval(root, registration);
    const auto ledgerPath = root / metadata / "inventory.json";
    const auto journalPath = root / metadata / "update.json";
    const auto packageId = digest(encode(added).dump() + helper);
    const bool recovering = pathExists(journalPath);
    Json before = nullptr, journal;
    if (recovering) {
        journal = load(journalPath);
        require(journal.is_object() && journal.size() == 4 && journal.at("format") == 1
                && journal.at("package") == packageId,
            "Rerun the same runtime package to recover this update.");
        before = journal.at("before");
    } else if (pathExists(ledgerPath)) {
        before = load(ledgerPath);
    } else {
        emptyDestination(root);
    }
    auto combined = added;
    Inventory previous;
    if (!before.is_null()) {
        previous = stateInventory(before, registration);
        verify(root, previous);
        combined = merge(previous, added);
    }
    const auto after = state(combined, helper, registration);
    if (recovering) {
        require(journal.at("after") == after, "Runtime recovery inventory changed.");
    }
    checkAdditions(root, added, before.is_null() ? nullptr : &previous, recovering);
    checkMetadata(root, before, after, recovering);
    if (!recovering) {
        journal = { { "format", 1 }, { "package", packageId }, { "before", before },
            { "after", after } };
        write(journalPath, journal.dump() + "\n", 0600);
    }
    for (const auto& name : combined.directories) {
        directory(root / fs::u8path(name));
    }
    for (const auto& [name, file] : added.files) {
        const auto target = root / fs::u8path(name);
        if (!pathExists(target)) {
            copyNew(package / "payload" / fs::u8path(name), target, file);
        }
    }
    verify(root, combined);
    write(root / "modules/modules.m", startupText(combined));
    write(root / "runtime.json", manifestText(combined));
    write(root / metadata / "uninstall", read(package / "runtime-installer"), 0755);
    write(ledgerPath, after.dump() + "\n");
    checkRegistration(registration, root, added.engine);
    if (!pathExists(registration)) {
        write(registration, registrationText(root));
    }
    require(fs::remove(journalPath), "Cannot retire runtime update journal.");
    std::cout << "Installed shared runtime: " << root.u8string() << '\n'
              << "Uninstaller: " << (root / metadata / "uninstall").u8string() << '\n';
}
//=============================================================================
} // namespace Nelson::RuntimeInstaller
//=============================================================================
