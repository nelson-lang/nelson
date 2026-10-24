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
#include <algorithm>
#include <iostream>
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
    constexpr const char* removalName = "remove.json";
    struct Removal
    {
        Json state;
        Inventory inventory;
        fs::path registration;
        bool complete = false;
    };
    //=============================================================================
    Removal
    removalState(const Json& state)
    {
        const auto registration = fs::u8path(state.at("registration").get<std::string>());
        safePath(registration);
        const auto inventory = stateInventory(state, registration);
        require(registration.filename() == inventory.engine + ".root"
                && registration.parent_path().filename() == architectureDirectory
                && registration.parent_path().parent_path().filename() == "runtimes"
                && registration.parent_path().parent_path().parent_path().filename() == "nelson",
            "Invalid runtime registration ownership.");
        return { state, inventory, registration, false };
    }
    //=============================================================================
    std::string
    removalText(const fs::path& root, const Removal& removal)
    {
        Json value = { { "format", 1 }, { "kind", "nelson-runtime-removal" },
            { "root", root.generic_u8string() }, { "state", removal.state },
            { "phase", removal.complete ? "complete" : "pending" } };
        value["sha256"] = digest(value.dump());
        return value.dump() + "\n";
    }
    //=============================================================================
    Removal
    readRemoval(const fs::path& root)
    {
        const auto path = root / metadata / removalName;
        const auto text = read(path);
        auto value = parse(text);
        require(value.is_object() && value.size() == 6 && value.at("format") == 1
                && value.at("kind") == "nelson-runtime-removal"
                && value.at("root") == root.generic_u8string()
                && (value.at("phase") == "pending" || value.at("phase") == "complete"),
            "Invalid runtime removal journal.");
        const auto expected = value.at("sha256").get<std::string>();
        value.erase("sha256");
        require(digest(value.dump()) == expected, "Runtime removal journal checksum differs.");
        auto result = removalState(value.at("state"));
        result.complete = value.at("phase") == "complete";
        require(text == removalText(root, result) && matches(path, { digest(text), 0600 }),
            "Runtime removal journal changed.");
        return result;
    }
    //=============================================================================
    void
    checkMetadata(const fs::path& root, const Removal& removal, bool recovering)
    {
        const std::map<std::string, File> expected = {
            { "modules/modules.m", { digest(startupText(removal.inventory)), 0644 } },
            { "runtime.json", { digest(manifestText(removal.inventory)), 0644 } },
            { ".nelson-runtime/inventory.json", { digest(removal.state.dump() + "\n"), 0644 } }
        };
        for (const auto& [name, file] : expected) {
            const auto path = root / name;
            require((recovering && !pathExists(path)) || matches(path, file),
                "Runtime uninstall metadata changed: " + name);
        }
        require(matches(root / metadata / "uninstall",
                    { removal.state.at("helperDigest").get<std::string>(), 0755 }),
            "Runtime uninstaller changed.");
    }
    //=============================================================================
    void
    removeHelper(const fs::path& root, const Removal& removal)
    {
        const auto helper = root / metadata / "uninstall";
        if (pathExists(helper)) {
            require(matches(helper, { removal.state.at("helperDigest").get<std::string>(), 0755 }),
                "Runtime uninstaller changed.");
            require(fs::remove(helper), "Cannot remove the runtime uninstaller.");
        }
    }
    //=============================================================================
    void
    removePayload(const fs::path& root, const Inventory& inventory)
    {
        for (const auto& [name, file] : inventory.files) {
            const auto path = root / fs::u8path(name);
            bool owned = false;
            try {
                owned = matches(path, file);
                if (!owned && pathExists(path)) {
                    std::cout << "Preserved modified file: " << name << '\n';
                }
            } catch (const std::exception& error) {
                std::cout << "Preserved redirected or inaccessible file: " << name << " ("
                          << error.what() << ")\n";
            }
            if (owned) {
                require(fs::remove(path), "Cannot remove runtime payload file: " + name);
            }
        }
    }
    //=============================================================================
    void
    removeDirectories(const fs::path& root, const Inventory& inventory)
    {
        std::vector<std::string> directories(
            inventory.directories.begin(), inventory.directories.end());
        std::sort(directories.begin(), directories.end(),
            [](const auto& a, const auto& b) { return a.size() > b.size(); });
        for (const auto& name : directories) {
            const auto path = root / fs::u8path(name);
            try {
                safePath(path);
                if (fs::is_directory(path) && fs::is_empty(path)) {
                    fs::remove(path);
                }
            } catch (const std::exception&) {
            }
        }
    }
    //=============================================================================
} // namespace
//=============================================================================
void
retireCompletedRemoval(const fs::path& root, const fs::path& registration)
{
    const auto journal = root / metadata / removalName;
    if (!pathExists(journal)) {
        return;
    }
    const auto removal = readRemoval(root);
    require(removal.complete, "Resume runtime uninstallation before installing another package.");
    require(!pathExists(registration), "Completed runtime removal registration changed.");
    for (const auto& entry : fs::directory_iterator(root)) {
        require(entry.path().filename() == metadata,
            "Runtime destination contains preserved or unmanaged files.");
    }
    for (const auto& entry : fs::directory_iterator(root / metadata)) {
        const auto name = entry.path().filename();
        require(name == "operation.lock" || name == removalName || name == "uninstall",
            "Unrecognized completed runtime removal data.");
    }
    removeHelper(root, removal);
    require(fs::remove(journal), "Cannot retire the completed runtime removal receipt.");
}
//=============================================================================
void
uninstall(const fs::path& root)
{
    safePath(root);
    const auto ledger = root / metadata / "inventory.json";
    const auto journal = root / metadata / removalName;
    const bool recovering = pathExists(journal);
    auto removal = recovering ? readRemoval(root) : removalState(parse(read(ledger)));
    directory(removal.registration.parent_path());
    Lock registered(removal.registration.string() + ".lock");
    Lock locked(root / metadata / "operation.lock");
    require(!pathExists(root / metadata / "update.json"),
        "Recover the runtime update before uninstalling.");
    require(recovering ? read(journal) == removalText(root, removal)
                       : (!pathExists(journal) && read(ledger) == removal.state.dump() + "\n"),
        "Runtime uninstall state changed.");
    if (!removal.complete) {
        checkMetadata(root, removal, recovering);
        if (!recovering) {
            write(journal, removalText(root, removal), 0600);
        }
        if (pathExists(removal.registration)
            && read(removal.registration, 32768) == registrationText(root)) {
            require(fs::remove(removal.registration), "Cannot remove runtime registration.");
        }
        removePayload(root, removal.inventory);
        for (const auto* name :
            { "modules/modules.m", "runtime.json", ".nelson-runtime/inventory.json" }) {
            fs::remove(root / name);
        }
        removeDirectories(root, removal.inventory);
        removal.complete = true;
        write(journal, removalText(root, removal), 0600);
    }
    // The receipt precedes self-removal, so a terminated completed process is unambiguous.
    removeHelper(root, removal);
    std::cout << "Removed shared runtime payload; user files, operation locks and completion "
                 "receipt were preserved: "
              << root.u8string() << '\n';
}
//=============================================================================
} // namespace Nelson::RuntimeInstaller
//=============================================================================
